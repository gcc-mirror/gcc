/* Copyright (C) 2001, 2002, 2003, 2004, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Written by Tom Tromey <tromey@redhat.com>  */

/* Uncomment this to enable debugging output.  */
/* #define VERIFY_DEBUG */

#include "config.h"

#include "verify.h"

/* Hack to work around namespace pollution from java-tree.h.  */
#undef current_class

#ifdef VERIFY_DEBUG
#include <stdio.h>
#endif /* VERIFY_DEBUG */

/* This is used to mark states which are not scheduled for
   verification. */
#define INVALID_STATE ((state *) -1)

static void ATTRIBUTE_PRINTF_1
debug_print (const char *fmt ATTRIBUTE_UNUSED, ...)
{
#ifdef VERIFY_DEBUG
  va_list ap;
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
#endif /* VERIFY_DEBUG */
}

/* This started as a fairly ordinary verifier, and for the most part
   it remains so.  It works in the obvious way, by modeling the effect
   of each opcode as it is encountered.  For most opcodes, this is a
   straightforward operation.

   This verifier does not do type merging.  It used to, but this
   results in difficulty verifying some relatively simple code
   involving interfaces, and it pushed some verification work into the
   interpreter.

   Instead of merging reference types, when we reach a point where two
   flows of control merge, we simply keep the union of reference types
   from each branch.  Then, when we need to verify a fact about a
   reference on the stack (e.g., that it is compatible with the
   argument type of a method), we check to ensure that all possible
   types satisfy the requirement.

   Another area this verifier differs from the norm is in its handling
   of subroutines.  The JVM specification has some confusing things to
   say about subroutines.  For instance, it makes claims about not
   allowing subroutines to merge and it rejects recursive subroutines.
   For the most part these are red herrings; we used to try to follow
   these things but they lead to problems.  For example, the notion of
   "being in a subroutine" is not well-defined: is an exception
   handler in a subroutine?  If you never execute the `ret' but
   instead `goto 1' do you remain in the subroutine?

   For clarity on what is really required for type safety, read
   "Simple Verification Technique for Complex Java Bytecode
   Subroutines" by Alessandro Coglio.  Among other things this paper
   shows that recursive subroutines are not harmful to type safety.
   We implement something similar to what he proposes.  Note that this
   means that this verifier will accept code that is rejected by some
   other verifiers.

   For those not wanting to read the paper, the basic observation is
   that we can maintain split states in subroutines.  We maintain one
   state for each calling `jsr'.  In other words, we re-verify a
   subroutine once for each caller, using the exact types held by the
   callers (as opposed to the old approach of merging types and
   keeping a bitmap registering what did or did not change).  This
   approach lets us continue to verify correctly even when a
   subroutine is exited via `goto' or `athrow' and not `ret'.

   In some other areas the JVM specification is (mildly) incorrect,
   so we diverge.  For instance, you cannot
   violate type safety by allocating an object with `new' and then
   failing to initialize it, no matter how one branches or where one
   stores the uninitialized reference.  See "Improving the official
   specification of Java bytecode verification" by Alessandro Coglio.

   Note that there's no real point in enforcing that padding bytes or
   the mystery byte of invokeinterface must be 0, but we do that
   regardless.

   The verifier is currently neither completely lazy nor eager when it
   comes to loading classes.  It tries to represent types by name when
   possible, and then loads them when it needs to verify a fact about
   the type.  Checking types by name is valid because we only use
   names which come from the current class' constant pool.  Since all
   such names are looked up using the same class loader, there is no
   danger that we might be fooled into comparing different types with
   the same name.

   In the future we plan to allow for a completely lazy mode of
   operation, where the verifier will construct a list of type
   assertions to be checked later.

   Some test cases for the verifier live in the "verify" module of the
   Mauve test suite.  However, some of these are presently
   (2004-01-20) believed to be incorrect.  (More precisely the notion
   of "correct" is not well-defined, and this verifier differs from
   others while remaining type-safe.)  Some other tests live in the
   libgcj test suite.

   This verifier is also written to be pluggable.  This means that it
   is intended for use in a variety of environments, not just libgcj.
   As a result the verifier expects a number of type and method
   declarations to be declared in "verify.h".  The intent is that you
   recompile the verifier for your particular environment.  This
   approach was chosen so that operations could be inlined in verify.h
   as much as possible.

   See the verify.h that accompanies this copy of the verifier to see
   what types, preprocessor defines, and functions must be declared.
   The interface is ad hoc, but was defined so that it could be
   implemented to connect to a pure C program.
*/

#define FLAG_INSN_START 1
#define FLAG_BRANCH_TARGET 2
#define FLAG_INSN_SEEN 4

struct state;
struct type;
struct ref_intersection;

typedef struct state state;
typedef struct type type;
typedef struct ref_intersection ref_intersection;

/*typedef struct state_list state_list;*/

typedef struct state_list
{
  state *val;
  struct state_list *next;
} state_list;

typedef struct vfy_string_list
{
  vfy_string val;
  struct vfy_string_list *next;
} vfy_string_list;

typedef struct verifier_context
{
  /* The current PC.  */
  int PC;
  /* The PC corresponding to the start of the current instruction.  */
  int start_PC;

  /* The current state of the stack, locals, etc.  */
  state *current_state;

  /* At each branch target we keep a linked list of all the states we
     can process at that point.  We'll only have multiple states at a
     given PC if they both have different return-address types in the
     same stack or local slot.  This array is indexed by PC and holds
     the list of all such states.  */
  state_list **states;

  /* We keep a linked list of all the states which we must reverify.
     This is the head of the list.  */
  state *next_verify_state;

  /* We keep some flags for each instruction.  The values are the
     FLAG_* constants defined above.  This is an array indexed by PC.  */
  char *flags;

  /* The bytecode itself.  */
  const unsigned char *bytecode;
  /* The exceptions.  */
  vfy_exception *exception;

  /* Defining class.  */
  vfy_jclass current_class;
  /* This method.  */
  vfy_method *current_method;

  /* A linked list of utf8 objects we allocate.  */
  vfy_string_list *utf8_list;

  /* A linked list of all ref_intersection objects we allocate.  */
  ref_intersection *isect_list;
} verifier_context;

/* The current verifier's state data. This is maintained by
   {push/pop}_verifier_context to provide a shorthand form to access
   the verification state. */
static GTY(()) verifier_context *vfr;

/* Local function declarations.  */
bool type_initialized (type *t);
int ref_count_dimensions (ref_intersection *ref);

static void
verify_fail_pc (const char *s, int pc)
{
  vfy_fail (s, pc, vfr->current_class, vfr->current_method);  
}

static void
verify_fail (const char *s)
{
  verify_fail_pc (s, vfr->PC);
}

/* This enum holds a list of tags for all the different types we
   need to handle.  Reference types are treated specially by the
   type class.  */
typedef enum type_val
{
  void_type,

  /* The values for primitive types are chosen to correspond to values
     specified to newarray. */
  boolean_type = 4,
  char_type = 5,
  float_type = 6,
  double_type = 7,
  byte_type = 8,
  short_type = 9,
  int_type = 10,
  long_type = 11,

  /* Used when overwriting second word of a double or long in the
     local variables.  Also used after merging local variable states
     to indicate an unusable value.  */
  unsuitable_type,
  return_address_type,
  /* This is the second word of a two-word value, i.e., a double or
     a long.  */
  continuation_type,

  /* Everything after `reference_type' must be a reference type.  */
  reference_type,
  null_type,
  uninitialized_reference_type
} type_val;

/* This represents a merged class type.  Some verifiers (including
   earlier versions of this one) will compute the intersection of
   two class types when merging states.  However, this loses
   critical information about interfaces implemented by the various
   classes.  So instead we keep track of all the actual classes that
   have been merged.  */
struct ref_intersection
{
  /* Whether or not this type has been resolved.  */
  bool is_resolved;

  /* Actual type data.  */
  union
  {
    /* For a resolved reference type, this is a pointer to the class.  */
    vfy_jclass klass;
    /* For other reference types, this it the name of the class.  */
    vfy_string name;
  } data;

  /* Link to the next reference in the intersection.  */
  ref_intersection *ref_next;

  /* This is used to keep track of all the allocated
     ref_intersection objects, so we can free them.
     FIXME: we should allocate these in chunks.  */
  ref_intersection *alloc_next;
};

static ref_intersection *
make_ref (void)
{
  ref_intersection *new_ref = 
    (ref_intersection *) vfy_alloc (sizeof (ref_intersection));

  new_ref->alloc_next = vfr->isect_list;
  vfr->isect_list = new_ref;
  return new_ref;
}

static ref_intersection *
clone_ref (ref_intersection *dup)
{
  ref_intersection *new_ref = make_ref ();

  new_ref->is_resolved = dup->is_resolved;
  new_ref->data = dup->data;
  return new_ref;
}

static void 
resolve_ref (ref_intersection *ref)
{
  if (ref->is_resolved)
    return;
  ref->data.klass = vfy_find_class (vfr->current_class, ref->data.name);
  ref->is_resolved = true;
}

static bool 
refs_equal (ref_intersection *ref1, ref_intersection *ref2)
{
  if (! ref1->is_resolved && ! ref2->is_resolved
      && vfy_strings_equal (ref1->data.name, ref2->data.name))
    return true;
  if (! ref1->is_resolved)
    resolve_ref (ref1);
  if (! ref2->is_resolved)
    resolve_ref (ref2);
  return ref1->data.klass == ref2->data.klass;
}

/* Merge REF1 type into REF2, returning the result.  This will
   return REF2 if all the classes in THIS already appear in
   REF2.  */
static ref_intersection *
merge_refs (ref_intersection *ref1, ref_intersection *ref2)
{
  ref_intersection *tail = ref2;
  for (; ref1 != NULL; ref1 = ref1->ref_next)
    {
      bool add = true;
      ref_intersection *iter;
      for (iter = ref2; iter != NULL; iter = iter->ref_next)
	{
	  if (refs_equal (ref1, iter))
	    {
	      add = false;
	      break;
	    }
	}

      if (add)
        {
	  ref_intersection *new_tail = clone_ref (ref1);
	  new_tail->ref_next = tail;
	  tail = new_tail;
	}
    }
  return tail;
}

/* See if an object of type SOURCE can be assigned to an object of
   type TARGET.  This might resolve classes in one chain or the other.  */
static bool 
ref_compatible (ref_intersection *target, ref_intersection *source)
{
  for (; target != NULL; target = target->ref_next)
    {
      ref_intersection *source_iter = source;

      for (; source_iter != NULL; source_iter = source_iter->ref_next)
	{
	  /* Avoid resolving if possible.  */
	  if (! target->is_resolved
	      && ! source_iter->is_resolved
	      && vfy_strings_equal (target->data.name,
				    source_iter->data.name))
	    continue;

	  if (! target->is_resolved)
	    resolve_ref (target);
	  if (! source_iter->is_resolved)
	    resolve_ref (source_iter);

	  if (! vfy_is_assignable_from (target->data.klass,
	  			        source_iter->data.klass))
	    return false;
	}
    }

  return true;
}

static bool 
ref_isarray (ref_intersection *ref)
{
  /* assert (ref_next == NULL);  */
  if (ref->is_resolved)
    return vfy_is_array (ref->data.klass);
  else
    return vfy_string_bytes (ref->data.name)[0] == '[';
}

static bool
ref_isinterface (ref_intersection *ref)
{
  /* assert (ref_next == NULL);  */
  if (! ref->is_resolved)
    resolve_ref (ref);
  return vfy_is_interface (ref->data.klass);
}

static bool
ref_isabstract (ref_intersection *ref)
{
  /* assert (ref_next == NULL); */
  if (! ref->is_resolved)
    resolve_ref (ref);
  return vfy_is_abstract (ref->data.klass);
}

static vfy_jclass 
ref_getclass (ref_intersection *ref)
{
  if (! ref->is_resolved)
    resolve_ref (ref);
  return ref->data.klass;
}

int
ref_count_dimensions (ref_intersection *ref)
{
  int ndims = 0;
  if (ref->is_resolved)
    {
      vfy_jclass k = ref->data.klass;
      while (vfy_is_array (k))
	{
 	  k = vfy_get_component_type (k);
	  ++ndims;
	}
    }
  else
    {
      const char *p = vfy_string_bytes (ref->data.name);
      while (*p++ == '[')
	++ndims;
    }
  return ndims;
}

/* Return the type_val corresponding to a primitive signature
   character.  For instance `I' returns `int.class'.  */
static type_val
get_type_val_for_signature (char sig)
{
  type_val rt;
  switch (sig)
    {
    case 'Z':
      rt = boolean_type;
      break;
    case 'B':
      rt = byte_type;
      break;
    case 'C':
      rt = char_type;
      break;
    case 'S':
      rt = short_type;
      break;
    case 'I':
      rt = int_type;
      break;
    case 'J':
      rt = long_type;
      break;
    case 'F':
      rt = float_type;
      break;
    case 'D':
      rt = double_type;
      break;
    case 'V':
      rt = void_type;
      break;
    default:
      verify_fail ("invalid signature");
      return null_type;
    }
  return rt;
}

/* Return the type_val corresponding to a primitive class.  */
static type_val
get_type_val_for_primtype (vfy_jclass k)
{
  return get_type_val_for_signature (vfy_get_primitive_char (k));
}

/* The `type' class is used to represent a single type in the verifier.  */
struct type
{
  /* The type key.  */
  type_val key;

  /* For reference types, the representation of the type.  */
  ref_intersection *klass;

  /* This is used in two situations.
  
     First, when constructing a new object, it is the PC of the
     `new' instruction which created the object.  We use the special
     value UNINIT to mean that this is uninitialized.  The special
     value SELF is used for the case where the current method is
     itself the <init> method.  the special value EITHER is used
     when we may optionally allow either an uninitialized or
     initialized reference to match.
  
     Second, when the key is return_address_type, this holds the PC
     of the instruction following the `jsr'.  */
  int pc;

#define UNINIT -2
#define SELF -1
#define EITHER -3
};

/* Make a new instance given the type tag.  We assume a generic
   `reference_type' means Object.  */
static void
init_type_from_tag (type *t, type_val k)
{
  t->key = k;
  /* For reference_type, if KLASS==NULL then that means we are
     looking for a generic object of any kind, including an
     uninitialized reference.  */
  t->klass = NULL;
  t->pc = UNINIT;
}

/* Make a type for the given type_val tag K.  */
static type
make_type (type_val k)
{
  type t;
  init_type_from_tag (&t, k);
  return t;
}

/* Make a new instance given a class.  */
static void
init_type_from_class (type *t, vfy_jclass k)
{
  t->key = reference_type;
  t->klass = make_ref ();
  t->klass->is_resolved = true;
  t->klass->data.klass = k;
  t->klass->ref_next = NULL;
  t->pc = UNINIT;
}

static type
make_type_from_class (vfy_jclass k)
{
  type t;
  init_type_from_class (&t, k);
  return t;
}

static void
init_type_from_string (type *t, vfy_string n)
{
  t->key = reference_type;
  t->klass = make_ref ();
  t->klass->is_resolved = false;
  t->klass->data.name = n;
  t->klass->ref_next = NULL;
  t->pc = UNINIT;
}

static type
make_type_from_string (vfy_string n)
{
  type t;
  init_type_from_string (&t, n);
  return t;
}

/* Promote a numeric type.  */
static void
vfy_promote_type (type *t)
{
  if (t->key == boolean_type || t->key == char_type
      || t->key == byte_type || t->key == short_type)
    t->key = int_type;
}
#define promote_type vfy_promote_type

/* Mark this type as the uninitialized result of `new'.  */
static void
type_set_uninitialized (type *t, int npc)
{
  if (t->key == reference_type)
    t->key = uninitialized_reference_type;
  else
    verify_fail ("internal error in type::uninitialized");
  t->pc = npc;
}

/* Mark this type as now initialized.  */
static void
type_set_initialized (type *t, int npc)
{
  if (npc != UNINIT && t->pc == npc && t->key == uninitialized_reference_type)
    {
      t->key = reference_type;
      t->pc = UNINIT;
    }
}

/* Mark this type as a particular return address.  */
static void type_set_return_address (type *t, int npc)
{
  t->pc = npc;
}

/* Return true if this type and type OTHER are considered
   mergeable for the purposes of state merging.  This is related
   to subroutine handling.  For this purpose two types are
   considered unmergeable if they are both return-addresses but
   have different PCs.  */
static bool
type_state_mergeable_p (type *t1, type *t2)
{
  return (t1->key != return_address_type
	  || t2->key != return_address_type
	  || t1->pc == t2->pc);
}

/* Return true if an object of type K can be assigned to a variable
   of type T.  Handle various special cases too.  Might modify
   T or K.  Note however that this does not perform numeric
   promotion.  */
static bool 
types_compatible (type *t, type *k)
{
  /* Any type is compatible with the unsuitable type.  */
  if (k->key == unsuitable_type)
    return true;

  if (t->key < reference_type || k->key < reference_type)
    return t->key == k->key;

  /* The `null' type is convertible to any initialized reference
     type.  */
  if (t->key == null_type)
    return k->key != uninitialized_reference_type;
  if (k->key == null_type)
    return t->key != uninitialized_reference_type;

  /* A special case for a generic reference.  */
  if (t->klass == NULL)
    return true;
  if (k->klass == NULL)
    verify_fail ("programmer error in type::compatible");

  /* Handle the special 'EITHER' case, which is only used in a
     special case of 'putfield'.  Note that we only need to handle
     this on the LHS of a check.  */
  if (! type_initialized (t) && t->pc == EITHER)
    {
      /* If the RHS is uninitialized, it must be an uninitialized
	 'this'.  */
      if (! type_initialized (k) && k->pc != SELF)
	return false;
    }
  else if (type_initialized (t) != type_initialized (k))
    {
      /* An initialized type and an uninitialized type are not
	 otherwise compatible.  */
      return false;
    }
  else
    {
      /* Two uninitialized objects are compatible if either:
       * The PCs are identical, or
       * One PC is UNINIT.  */
      if (type_initialized (t))
	{
	  if (t->pc != k->pc && t->pc != UNINIT && k->pc != UNINIT)
	    return false;
	}
    }

  return ref_compatible (t->klass, k->klass);
}

/* Return true if two types are equal.  Only valid for reference
   types.  */
static bool
types_equal (type *t1, type *t2)
{
  if ((t1->key != reference_type && t1->key != uninitialized_reference_type)
      || (t2->key != reference_type
	  && t2->key != uninitialized_reference_type))
    return false;
  /* Only single-ref types are allowed.  */
  if (t1->klass->ref_next || t2->klass->ref_next)
    return false;
  return refs_equal (t1->klass, t2->klass);
}

static bool
type_isvoid (type *t)
{
  return t->key == void_type;
}

static bool
type_iswide (type *t)
{
  return t->key == long_type || t->key == double_type;
}

/* Return number of stack or local variable slots taken by this type.  */  
static int
type_depth (type *t)
{
  return type_iswide (t) ? 2 : 1;
}

static bool
type_isarray (type *t)
{
  /* We treat null_type as not an array.  This is ok based on the
     current uses of this method.  */
  if (t->key == reference_type)
    return ref_isarray (t->klass);
  return false;
}

static bool
type_isnull (type *t)
{
  return t->key == null_type;
}

static bool
type_isinterface (type *t)
{
  if (t->key != reference_type)
    return false;
  return ref_isinterface (t->klass);
}

static bool
type_isabstract (type *t)
{
  if (t->key != reference_type)
    return false;
  return ref_isabstract (t->klass);
}

/* Return the element type of an array.  */
static type
type_array_element (type *t)
{
  type et;
  vfy_jclass k;

  if (t->key != reference_type)
    verify_fail ("programmer error in type::element_type()");

  k = vfy_get_component_type (ref_getclass (t->klass));
  if (vfy_is_primitive (k))
    init_type_from_tag (&et, get_type_val_for_primtype (k));
  else
    init_type_from_class (&et, k);
  return et;
}

/* Return the array type corresponding to an initialized
   reference.  We could expand this to work for other kinds of
   types, but currently we don't need to.  */
static type 
type_to_array (type *t)
{
  type at;
  vfy_jclass k;

  if (t->key != reference_type)
    verify_fail ("internal error in type::to_array()");

  k = ref_getclass (t->klass);
  init_type_from_class (&at, vfy_get_array_class (k));
  return at;
}

static bool
type_isreference (type *t)
{
  return t->key >= reference_type;
}

static int
type_get_pc (type *t)
{
  return t->pc;
}

bool
type_initialized (type *t)
{
  return t->key == reference_type || t->key == null_type;
}

static void
type_verify_dimensions (type *t, int ndims)
{
  /* The way this is written, we don't need to check isarray().  */
  if (t->key != reference_type)
    verify_fail ("internal error in verify_dimensions:"
			   " not a reference type");

  if (ref_count_dimensions (t->klass) < ndims)
    verify_fail ("array type has fewer dimensions"
			   " than required");
}

/* Merge OLD_TYPE into this.  On error throw exception.  Return
   true if the merge caused a type change.  */
static bool
merge_types (type *t, type *old_type, bool local_semantics)
{
  bool changed = false;
  bool refo = type_isreference (old_type);
  bool refn = type_isreference (t);
  if (refo && refn)
    {
      if (old_type->key == null_type)
	;
      else if (t->key == null_type)
	{
	  *t = *old_type;
	  changed = true;
	}
      else if (type_initialized (t) != type_initialized (old_type))
	verify_fail ("merging initialized and uninitialized types");
      else
	{
	  ref_intersection *merged;
	  if (! type_initialized (t))
	    {
	      if (t->pc == UNINIT)
		t->pc = old_type->pc;
	      else if (old_type->pc == UNINIT)
		;
	      else if (t->pc != old_type->pc)
		verify_fail ("merging different uninitialized types");
	    }

	  merged = merge_refs (old_type->klass, t->klass);
	  if (merged != t->klass)
	    {
	      t->klass = merged;
	      changed = true;
	    }
	}
    }
  else if (refo || refn || t->key != old_type->key)
    {
      if (local_semantics)
	{
	  /* If we already have an `unsuitable' type, then we
	     don't need to change again.  */
	  if (t->key != unsuitable_type)
	    {
	      t->key = unsuitable_type;
	      changed = true;
	    }
	}
      else
	verify_fail ("unmergeable type");
    }
  return changed;
}

#ifdef VERIFY_DEBUG
static void 
type_print (type *t)
{
  char c = '?';
  switch (t->key)
    {
    case boolean_type: c = 'Z'; break;
    case byte_type: c = 'B'; break;
    case char_type: c = 'C'; break;
    case short_type: c = 'S'; break;
    case int_type: c = 'I'; break;
    case long_type: c = 'J'; break;
    case float_type: c = 'F'; break;
    case double_type: c = 'D'; break;
    case void_type: c = 'V'; break;
    case unsuitable_type: c = '-'; break;
    case return_address_type: c = 'r'; break;
    case continuation_type: c = '+'; break;
    case reference_type: c = 'L'; break;
    case null_type: c = '@'; break;
    case uninitialized_reference_type: c = 'U'; break;
    }
  debug_print ("%c", c);
}
#endif /* VERIFY_DEBUG */

/* This class holds all the state information we need for a given
   location. */
struct state
{
  /* The current top of the stack, in terms of slots.  */
  int stacktop;
  /* The current depth of the stack.  This will be larger than
     STACKTOP when wide types are on the stack.  */
  int stackdepth;
  /* The stack.  */
  type *stack;
  /* The local variables.  */
  type *locals;
  /* We keep track of the type of `this' specially.  This is used to
     ensure that an instance initializer invokes another initializer
     on `this' before returning.  We must keep track of this
     specially because otherwise we might be confused by code which
     assigns to locals[0] (overwriting `this') and then returns
     without really initializing.  */
  type this_type;

  /* The PC for this state.  This is only valid on states which are
     permanently attached to a given PC.  For an object like
     `current_state', which is used transiently, this has no
     meaning.  */
  int pc;
  /* We keep a linked list of all states requiring reverification.
     If this is the special value INVALID_STATE then this state is
     not on the list.  NULL marks the end of the linked list.  */
  state *next;
};

/* NO_NEXT is the PC value meaning that a new state must be
   acquired from the verification list.  */
#define NO_NEXT -1

static void
init_state_with_stack (state *s, int max_stack, int max_locals)
{
  int i;
  s->stacktop = 0;
  s->stackdepth = 0;
  s->stack = (type *) vfy_alloc (max_stack * sizeof (type));
  for (i = 0; i < max_stack; ++i)
    init_type_from_tag (&s->stack[i], unsuitable_type);
  s->locals = (type *) vfy_alloc (max_locals * sizeof (type));
  for (i = 0; i < max_locals; ++i)
    init_type_from_tag (&s->locals[i], unsuitable_type);
  init_type_from_tag (&s->this_type, unsuitable_type);
  s->pc = NO_NEXT;
  s->next = INVALID_STATE;
}

static void
copy_state (state *s, state *copy, int max_stack, int max_locals)
{
  int i;
  s->stacktop = copy->stacktop;
  s->stackdepth = copy->stackdepth;
  for (i = 0; i < max_stack; ++i)
    s->stack[i] = copy->stack[i];
  for (i = 0; i < max_locals; ++i)
    s->locals[i] = copy->locals[i];

  s->this_type = copy->this_type;
  /* Don't modify `next' or `pc'. */
}

static void
copy_state_with_stack (state *s, state *orig, int max_stack, int max_locals)
{
  init_state_with_stack (s, max_stack, max_locals);
  copy_state (s, orig, max_stack, max_locals);
}

/* Allocate a new state, copying ORIG. */
static state *
make_state_copy (state *orig, int max_stack, int max_locals)
{
  state *s = vfy_alloc (sizeof (state));
  copy_state_with_stack (s, orig, max_stack, max_locals);
  return s;
}

static state *
make_state (int max_stack, int max_locals)
{
  state *s = vfy_alloc (sizeof (state));
  init_state_with_stack (s, max_stack, max_locals);
  return s;
}

static void
free_state (state *s)
{
  if (s->stack != NULL)
    vfy_free (s->stack);
  if (s->locals != NULL)
    vfy_free (s->locals);
}

/* Modify this state to reflect entry to an exception handler.  */
static void
state_set_exception (state *s, type *t, int max_stack)
{
  int i;
  s->stackdepth = 1;
  s->stacktop = 1;
  s->stack[0] = *t;
  for (i = s->stacktop; i < max_stack; ++i)
    init_type_from_tag (&s->stack[i], unsuitable_type);
}

/* Merge STATE_OLD into this state.  Destructively modifies this
   state.  Returns true if the new state was in fact changed.
   Will throw an exception if the states are not mergeable.  */
static bool
merge_states (state *s, state *state_old, int max_locals)
{
  int i;
  bool changed = false;

  /* Special handling for `this'.  If one or the other is
     uninitialized, then the merge is uninitialized.  */
  if (type_initialized (&s->this_type))
    s->this_type = state_old->this_type;

  /* Merge stacks.  */
  if (state_old->stacktop != s->stacktop)  /* FIXME stackdepth instead?  */
    verify_fail ("stack sizes differ");
  for (i = 0; i < state_old->stacktop; ++i)
    {
      if (merge_types (&s->stack[i], &state_old->stack[i], false))
	changed = true;
    }

  /* Merge local variables.  */
  for (i = 0; i < max_locals; ++i)
    {
      if (merge_types (&s->locals[i], &state_old->locals[i], true))
	changed = true;
    }

  return changed;
}

/* Ensure that `this' has been initialized.  */
static void
state_check_this_initialized (state *s)
{
  if (type_isreference (&s->this_type) && ! type_initialized (&s->this_type))
    verify_fail ("`this' is uninitialized");
}

/* Set type of `this'.  */
static void
state_set_this_type (state *s, type *k)
{
  s->this_type = *k;
}

/* Mark each `new'd object we know of that was allocated at PC as
   initialized.  */
static void
state_set_initialized (state *s, int pc, int max_locals)
{
  int i;
  for (i = 0; i < s->stacktop; ++i)
    type_set_initialized (&s->stack[i], pc);
  for (i = 0; i < max_locals; ++i)
    type_set_initialized (&s->locals[i], pc);
  type_set_initialized (&s->this_type, pc);
}

/* This tests to see whether two states can be considered "merge
   compatible".  If both states have a return-address in the same
   slot, and the return addresses are different, then they are not
   compatible and we must not try to merge them.  */
static bool
state_mergeable_p (state *s, state *other, int max_locals)

{
  int i;

  /* This is tricky: if the stack sizes differ, then not only are
     these not mergeable, but in fact we should give an error, as
     we've found two execution paths that reach a branch target
     with different stack depths.  FIXME stackdepth instead?  */
  if (s->stacktop != other->stacktop)
    verify_fail ("stack sizes differ");

  for (i = 0; i < s->stacktop; ++i)
    if (! type_state_mergeable_p (&s->stack[i], &other->stack[i]))
      return false;
  for (i = 0; i < max_locals; ++i)
    if (! type_state_mergeable_p (&s->locals[i], &other->locals[i]))
      return false;
  return true;
}

static void
state_reverify (state *s)
{
  if (s->next == INVALID_STATE)
    {
      s->next = vfr->next_verify_state;
      vfr->next_verify_state = s;
    }
}

#ifdef VERIFY_DEBUG
static void 
debug_print_state (state *s, const char *leader, int pc, int max_stack, 
		   int max_locals)
{
  int i;
  debug_print ("%s [%4d]:   [stack] ", leader, pc);
  for (i = 0; i < s->stacktop; ++i)
    type_print (&s->stack[i]);
  for (; i < max_stack; ++i)
    debug_print (".");
  debug_print ("    [local] ");
  for (i = 0; i < max_locals; ++i)
    type_print (&s->locals[i]);
  debug_print (" | %p\n", s);
}
#else
static void
debug_print_state (state *s ATTRIBUTE_UNUSED, 
		   const char *leader ATTRIBUTE_UNUSED, 
		   int pc ATTRIBUTE_UNUSED, int max_stack ATTRIBUTE_UNUSED, 
		   int max_locals ATTRIBUTE_UNUSED)
{
}
#endif /* VERIFY_DEBUG */

static type
pop_raw (void)
{
  type r;
  state *s = vfr->current_state;
  if (s->stacktop <= 0)
    verify_fail ("stack empty");
  r = s->stack[--s->stacktop];
  s->stackdepth -= type_depth (&r);
  if (s->stackdepth < 0)
    verify_fail_pc ("stack empty", vfr->start_PC);
  return r;
}

static type
pop32 (void)
{
  type r = pop_raw ();
  if (type_iswide (&r))
    verify_fail ("narrow pop of wide type");
  return r;
}

static type
vfy_pop_type_t (type match)
{
  type t;
  vfy_promote_type (&match);
  t = pop_raw ();
  if (! types_compatible (&match, &t))
    verify_fail ("incompatible type on stack");
  return t;
}

static type
vfy_pop_type (type_val match)
{
  type t = make_type (match);
  return vfy_pop_type_t (t);
}

#define pop_type vfy_pop_type
#define pop_type_t vfy_pop_type_t

/* Pop a reference which is guaranteed to be initialized.  MATCH
   doesn't have to be a reference type; in this case this acts like
   pop_type.  */
static type
pop_init_ref_t (type match)
{
  type t = pop_raw ();
  if (type_isreference (&t) && ! type_initialized (&t))
    verify_fail ("initialized reference required");
  else if (! types_compatible (&match, &t))
    verify_fail ("incompatible type on stack");
  return t;
}

static type
pop_init_ref (type_val match)
{
  type t = make_type (match);
  return pop_init_ref_t (t);
}

/* Pop a reference type or a return address.  */
static type
pop_ref_or_return (void)
{
  type t = pop_raw ();
  if (! type_isreference (&t) && t.key != return_address_type)
    verify_fail ("expected reference or return address on stack");
  return t;
}

static void
vfy_push_type_t (type t)
{
  int depth;
  state *s = vfr->current_state;
  /* If T is a numeric type like short, promote it to int.  */
  promote_type (&t);

  depth = type_depth (&t);

  if (s->stackdepth + depth > vfr->current_method->max_stack)
    verify_fail ("stack overflow");
  s->stack[s->stacktop++] = t;
  s->stackdepth += depth;
}

static void
vfy_push_type (type_val tval)
{
  type t = make_type (tval);
  vfy_push_type_t (t);
}

#define push_type vfy_push_type
#define push_type_t vfy_push_type_t

static void
set_variable (int index, type t)
{
  int depth;
  state *s = vfr->current_state;
  /* If T is a numeric type like short, promote it to int.  */
  promote_type (&t);

  depth = type_depth (&t);
  if (index > vfr->current_method->max_locals - depth)
    verify_fail ("invalid local variable");
  s->locals[index] = t;

  if (depth == 2)
    init_type_from_tag (&s->locals[index + 1], continuation_type);
  if (index > 0 && type_iswide (&s->locals[index - 1]))
    init_type_from_tag (&s->locals[index - 1], unsuitable_type);
}

static type
get_variable_t (int index, type *t)
{
  state *s = vfr->current_state;
  int depth = type_depth (t);
  if (index > vfr->current_method->max_locals - depth)
    verify_fail ("invalid local variable");
  if (! types_compatible (t, &s->locals[index]))
    verify_fail ("incompatible type in local variable");
  if (depth == 2)
    {
      type cont = make_type (continuation_type);
      if (! types_compatible (&s->locals[index + 1], &cont))
	verify_fail ("invalid local variable");
    }
  return s->locals[index];
}

static type
get_variable (int index, type_val v)
{
  type t = make_type (v);
  return get_variable_t (index, &t);
}

/* Make sure ARRAY is an array type and that its elements are
   compatible with type ELEMENT.  Returns the actual element type.  */
static type
require_array_type_t (type array, type element)
{
  type t;
  /* An odd case.  Here we just pretend that everything went ok.  If
     the requested element type is some kind of reference, return
     the null type instead.  */
  if (type_isnull (&array))
    return type_isreference (&element) ? make_type (null_type) : element;

  if (! type_isarray (&array))
    verify_fail ("array required");

  t = type_array_element (&array);
  if (! types_compatible (&element, &t))
    {
      /* Special case for byte arrays, which must also be boolean
         arrays.  */
      bool ok = true;
      if (element.key == byte_type)
	{
	  type e2 = make_type (boolean_type);
	  ok = types_compatible (&e2, &t);
	}
      if (! ok)
	verify_fail ("incompatible array element type");
    }

  /* Return T and not ELEMENT, because T might be specialized.  */
  return t;
}

static type
require_array_type (type array, type_val element)
{
  type t = make_type (element);
  return require_array_type_t (array, t);
}

static jint
get_byte (void)
{
  if (vfr->PC >= vfr->current_method->code_length)
    verify_fail ("premature end of bytecode");
  return (jint) vfr->bytecode[vfr->PC++] & 0xff;
}

static jint
get_ushort (void)
{
  jint b1 = get_byte ();
  jint b2 = get_byte ();
  return (jint) ((b1 << 8) | b2) & 0xffff;
}

static jint
get_short (void)
{
  signed char b1 = (signed char) get_byte ();
  jint b2 = get_byte ();
  jshort s = (b1 << 8) | b2;
  return (jint) s;
}

static jint
get_int (void)
{
  jint b1 = get_byte ();
  jint b2 = get_byte ();
  jint b3 = get_byte ();
  jint b4 = get_byte ();
  jword result = (b1 << 24) | (b2 << 16) | (b3 << 8) | b4;
  /* In the compiler, 'jint' might have more than 32 bits, so we must
     sign extend.  */
  return WORD_TO_INT (result);
}

static int
compute_jump (int offset)
{
  int npc = vfr->start_PC + offset;
  if (npc < 0 || npc >= vfr->current_method->code_length)
    verify_fail_pc ("branch out of range", vfr->start_PC);
  return npc;
}

/* Add a new state to the state list at NPC.  */
static state *
add_new_state (int npc, state *old_state)
{
  state_list *nlink;
  vfy_method *current_method = vfr->current_method;
  state *new_state = make_state_copy (old_state, current_method->max_stack,
				      current_method->max_locals);
  debug_print ("== New state in add_new_state\n");
  debug_print_state (new_state, "New", npc, current_method->max_stack,
		    current_method->max_locals);

  nlink = vfy_alloc (sizeof (state_list));
  nlink->val = new_state;
  nlink->next = vfr->states[npc];
  vfr->states[npc] = nlink;
  new_state->pc = npc;
  return new_state;
}

/* Merge the indicated state into the state at the branch target and
   schedule a new PC if there is a change.  NPC is the PC of the
   branch target, and FROM_STATE is the state at the source of the
   branch.  This method returns true if the destination state
   changed and requires reverification, false otherwise.  */
static void
merge_into (int npc, state *from_state)
{
  /* Iterate over all target states and merge our state into each,
     if applicable.  FIXME one improvement we could make here is
     "state destruction".  Merging a new state into an existing one
     might cause a return_address_type to be merged to
     unsuitable_type.  In this case the resulting state may now be
     mergeable with other states currently held in parallel at this
     location.  So in this situation we could pairwise compare and
     reduce the number of parallel states.  */
  state_list *iter;
  bool applicable = false;
  for (iter = vfr->states[npc]; iter != NULL; iter = iter->next)
    {
      state *new_state = iter->val;
      vfy_method *current_method = vfr->current_method;

      if (state_mergeable_p (new_state, from_state,
					current_method->max_locals))
	{
	  bool changed;
	  applicable = true;

	  debug_print ("== Merge states in merge_into\n");
	  debug_print_state (from_state, "Frm", vfr->start_PC, current_method->max_stack,
			     current_method->max_locals);
	  debug_print_state (new_state, " To", npc, current_method->max_stack,
			    current_method->max_locals);
	  changed = merge_states (new_state, from_state,
				  current_method->max_locals);
	  debug_print_state (new_state, "New", npc, current_method->max_stack,
			    current_method->max_locals);

	  if (changed)
	    state_reverify (new_state);
	}
    }

  if (! applicable)
    {
      /* Either we don't yet have a state at NPC, or we have a
         return-address type that is in conflict with all existing
         state.  So, we need to create a new entry.  */
      state *new_state = add_new_state (npc, from_state);
      /* A new state added in this way must always be reverified.  */
      state_reverify (new_state);
    }
}

static void
push_jump (int offset)
{
  int npc = compute_jump (offset);
  /* According to the JVM Spec, we need to check for uninitialized
     objects here.  However, this does not actually affect type
     safety, and the Eclipse java compiler generates code that
     violates this constraint.  */
  merge_into (npc, vfr->current_state);
}

static void
push_exception_jump (type t, int pc)
{
  state s;
  /* According to the JVM Spec, we need to check for uninitialized
     objects here.  However, this does not actually affect type
     safety, and the Eclipse java compiler generates code that
     violates this constraint.  */
  copy_state_with_stack (&s, vfr->current_state, 
			 vfr->current_method->max_stack,
			 vfr->current_method->max_locals);
  if (vfr->current_method->max_stack < 1)
    verify_fail ("stack overflow at exception handler");
  state_set_exception (&s, &t, vfr->current_method->max_stack);
  merge_into (pc, &s);
  /* FIXME: leak.. need free_state or GC */
}

static state *
pop_jump (void)
{
  state *new_state = vfr->next_verify_state;
  if (new_state == INVALID_STATE)
    verify_fail ("programmer error in pop_jump");
  if (new_state != NULL)
    {
      vfr->next_verify_state = new_state->next;
      new_state->next = INVALID_STATE;
    }
  return new_state;
}

static void
invalidate_pc (void)
{
  vfr->PC = NO_NEXT;
}

static void
note_branch_target (int pc)
{
  /* Don't check `pc <= PC', because we've advanced PC after
     fetching the target and we haven't yet checked the next
     instruction.  */
  if (pc < vfr->PC && ! (vfr->flags[pc] & FLAG_INSN_START))
    verify_fail_pc ("branch not to instruction start", vfr->start_PC);
  vfr->flags[pc] |= FLAG_BRANCH_TARGET;
}

static void
skip_padding (void)
{
  while ((vfr->PC % 4) > 0)
    if (get_byte () != 0)
      verify_fail ("found nonzero padding byte");
}

/* Do the work for a `ret' instruction.  INDEX is the index into the
   local variables.  */
static void
handle_ret_insn (int index)
{
  type ret = make_type (return_address_type);
  type ret_addr = get_variable_t (index, &ret);
  /* It would be nice if we could do this.  However, the JVM Spec
     doesn't say that this is what happens.  It is implied that
     reusing a return address is invalid, but there's no actual
     prohibition against it.  */
  /* set_variable (index, unsuitable_type); */

  int npc = type_get_pc (&ret_addr);
  /* We might be returning to a `jsr' that is at the end of the
     bytecode.  This is ok if we never return from the called
     subroutine, but if we see this here it is an error.  */
  if (npc >= vfr->current_method->code_length)
    verify_fail ("fell off end");

  /* According to the JVM Spec, we need to check for uninitialized
     objects here.  However, this does not actually affect type
     safety, and the Eclipse java compiler generates code that
     violates this constraint.  */
  merge_into (npc, vfr->current_state);
  invalidate_pc ();
}

static void handle_jsr_insn (int offset)
{
  type ret_addr;
  int npc = compute_jump (offset);

  /* According to the JVM Spec, we need to check for uninitialized
     objects here.  However, this does not actually affect type
     safety, and the Eclipse java compiler generates code that
     violates this constraint.  */

  /* Modify our state as appropriate for entry into a subroutine.  */
  ret_addr = make_type (return_address_type);
  type_set_return_address (&ret_addr, vfr->PC);
  vfy_push_type_t (ret_addr);
  merge_into (npc, vfr->current_state);
  invalidate_pc ();
}

static vfy_jclass
construct_primitive_array_type (type_val prim)
{
  vfy_jclass k = NULL;
  switch (prim)
    {
    case boolean_type:
    case char_type:
    case float_type:
    case double_type:
    case byte_type:
    case short_type:
    case int_type:
    case long_type:
      k = vfy_get_primitive_type ((int) prim);
      break;

    /* These aren't used here but we call them out to avoid
       warnings.  */
    case void_type:
    case unsuitable_type:
    case return_address_type:
    case continuation_type:
    case reference_type:
    case null_type:
    case uninitialized_reference_type:
    default:
      verify_fail ("unknown type in construct_primitive_array_type");
    }
  k = vfy_get_array_class (k);
  return k;
}

/* This pass computes the location of branch targets and also
   instruction starts.  */
static void
branch_prepass (void)
{
  int i, pc;
  vfr->flags = (char *) vfy_alloc (vfr->current_method->code_length);

  for (i = 0; i < vfr->current_method->code_length; ++i)
    vfr->flags[i] = 0;

  vfr->PC = 0;
  while (vfr->PC < vfr->current_method->code_length)
    {
      java_opcode opcode;
      /* Set `start_PC' early so that error checking can have the
         correct value.  */
      vfr->start_PC = vfr->PC;
      vfr->flags[vfr->PC] |= FLAG_INSN_START;

      opcode = (java_opcode) vfr->bytecode[vfr->PC++];
      switch (opcode)
	{
	case op_nop:
	case op_aconst_null:
	case op_iconst_m1:
	case op_iconst_0:
	case op_iconst_1:
	case op_iconst_2:
	case op_iconst_3:
	case op_iconst_4:
	case op_iconst_5:
	case op_lconst_0:
	case op_lconst_1:
	case op_fconst_0:
	case op_fconst_1:
	case op_fconst_2:
	case op_dconst_0:
	case op_dconst_1:
	case op_iload_0:
	case op_iload_1:
	case op_iload_2:
	case op_iload_3:
	case op_lload_0:
	case op_lload_1:
	case op_lload_2:
	case op_lload_3:
	case op_fload_0:
	case op_fload_1:
	case op_fload_2:
	case op_fload_3:
	case op_dload_0:
	case op_dload_1:
	case op_dload_2:
	case op_dload_3:
	case op_aload_0:
	case op_aload_1:
	case op_aload_2:
	case op_aload_3:
	case op_iaload:
	case op_laload:
	case op_faload:
	case op_daload:
	case op_aaload:
	case op_baload:
	case op_caload:
	case op_saload:
	case op_istore_0:
	case op_istore_1:
	case op_istore_2:
	case op_istore_3:
	case op_lstore_0:
	case op_lstore_1:
	case op_lstore_2:
	case op_lstore_3:
	case op_fstore_0:
	case op_fstore_1:
	case op_fstore_2:
	case op_fstore_3:
	case op_dstore_0:
	case op_dstore_1:
	case op_dstore_2:
	case op_dstore_3:
	case op_astore_0:
	case op_astore_1:
	case op_astore_2:
	case op_astore_3:
	case op_iastore:
	case op_lastore:
	case op_fastore:
	case op_dastore:
	case op_aastore:
	case op_bastore:
	case op_castore:
	case op_sastore:
	case op_pop:
	case op_pop2:
	case op_dup:
	case op_dup_x1:
	case op_dup_x2:
	case op_dup2:
	case op_dup2_x1:
	case op_dup2_x2:
	case op_swap:
	case op_iadd:
	case op_isub:
	case op_imul:
	case op_idiv:
	case op_irem:
	case op_ishl:
	case op_ishr:
	case op_iushr:
	case op_iand:
	case op_ior:
	case op_ixor:
	case op_ladd:
	case op_lsub:
	case op_lmul:
	case op_ldiv:
	case op_lrem:
	case op_lshl:
	case op_lshr:
	case op_lushr:
	case op_land:
	case op_lor:
	case op_lxor:
	case op_fadd:
	case op_fsub:
	case op_fmul:
	case op_fdiv:
	case op_frem:
	case op_dadd:
	case op_dsub:
	case op_dmul:
	case op_ddiv:
	case op_drem:
	case op_ineg:
	case op_i2b:
	case op_i2c:
	case op_i2s:
	case op_lneg:
	case op_fneg:
	case op_dneg:
	case op_i2l:
	case op_i2f:
	case op_i2d:
	case op_l2i:
	case op_l2f:
	case op_l2d:
	case op_f2i:
	case op_f2l:
	case op_f2d:
	case op_d2i:
	case op_d2l:
	case op_d2f:
	case op_lcmp:
	case op_fcmpl:
	case op_fcmpg:
	case op_dcmpl:
	case op_dcmpg:
	case op_monitorenter:
	case op_monitorexit:
	case op_ireturn:
	case op_lreturn:
	case op_freturn:
	case op_dreturn:
	case op_areturn:
	case op_return:
	case op_athrow:
	case op_arraylength:
	  break;

	case op_bipush:
	case op_ldc:
	case op_iload:
	case op_lload:
	case op_fload:
	case op_dload:
	case op_aload:
	case op_istore:
	case op_lstore:
	case op_fstore:
	case op_dstore:
	case op_astore:
	case op_ret:
	case op_newarray:
	  get_byte ();
	  break;

	case op_iinc:
	case op_sipush:
	case op_ldc_w:
	case op_ldc2_w:
	case op_getstatic:
	case op_getfield:
	case op_putfield:
	case op_putstatic:
	case op_new:
	case op_anewarray:
	case op_instanceof:
	case op_checkcast:
	case op_invokespecial:
	case op_invokestatic:
	case op_invokevirtual:
	  get_short ();
	  break;

	case op_multianewarray:
	  get_short ();
	  get_byte ();
	  break;

	case op_jsr:
	case op_ifeq:
	case op_ifne:
	case op_iflt:
	case op_ifge:
	case op_ifgt:
	case op_ifle:
	case op_if_icmpeq:
	case op_if_icmpne:
	case op_if_icmplt:
	case op_if_icmpge:
	case op_if_icmpgt:
	case op_if_icmple:
	case op_if_acmpeq:
	case op_if_acmpne:
	case op_ifnull:
	case op_ifnonnull:
	case op_goto:
	  note_branch_target (compute_jump (get_short ()));
	  break;

	case op_tableswitch:
	  {
	    jint low, hi;
	    skip_padding ();
	    note_branch_target (compute_jump (get_int ()));
	    low = get_int ();
	    hi = get_int ();
	    if (low > hi)
	      verify_fail_pc ("invalid tableswitch", vfr->start_PC);
	    for (i = low; i <= hi; ++i)
	      note_branch_target (compute_jump (get_int ()));
	  }
	  break;

	case op_lookupswitch:
	  {
	    int npairs;
	    skip_padding ();
	    note_branch_target (compute_jump (get_int ()));
	    npairs = get_int ();
	    if (npairs < 0)
	      verify_fail_pc ("too few pairs in lookupswitch", vfr->start_PC);
	    while (npairs-- > 0)
	      {
		get_int ();
		note_branch_target (compute_jump (get_int ()));
	      }
	  }
	  break;

	case op_invokeinterface:
	  get_short ();
	  get_byte ();
	  get_byte ();
	  break;

	case op_wide:
	  {
	    opcode = (java_opcode) get_byte ();
	    get_short ();
	    if (opcode == op_iinc)
	      get_short ();
	  }
	  break;

	case op_jsr_w:
	case op_goto_w:
	  note_branch_target (compute_jump (get_int ()));
	  break;

#if 0
	/* These are unused here, but we call them out explicitly
	   so that -Wswitch-enum doesn't complain.  */
	case op_putfield_1:
	case op_putfield_2:
	case op_putfield_4:
	case op_putfield_8:
	case op_putfield_a:
	case op_putstatic_1:
	case op_putstatic_2:
	case op_putstatic_4:
	case op_putstatic_8:
	case op_putstatic_a:
	case op_getfield_1:
	case op_getfield_2s:
	case op_getfield_2u:
	case op_getfield_4:
	case op_getfield_8:
	case op_getfield_a:
	case op_getstatic_1:
	case op_getstatic_2s:
	case op_getstatic_2u:
	case op_getstatic_4:
	case op_getstatic_8:
	case op_getstatic_a:
#endif /* VFY_FAST_OPCODES  */
	default:
	  verify_fail_pc ("unrecognized instruction in branch_prepass",
			  vfr->start_PC);
	}

      /* See if any previous branch tried to branch to the middle of
         this instruction.  */
      for (pc = vfr->start_PC + 1; pc < vfr->PC; ++pc)
	{
	  if ((vfr->flags[pc] & FLAG_BRANCH_TARGET))
	    verify_fail_pc ("branch to middle of instruction", pc);
	}
    }

  /* Verify exception handlers.  */
  for (i = 0; i < vfr->current_method->exc_count; ++i)
    {
      int handler, start, end, htype;
      vfy_get_exception (vfr->exception, i, &handler, &start, &end, &htype);
      if (! (vfr->flags[handler] & FLAG_INSN_START))
	verify_fail_pc ("exception handler not at instruction start", 
			handler);
      if (! (vfr->flags[start] & FLAG_INSN_START))
	verify_fail_pc ("exception start not at instruction start", start);
      if (end != vfr->current_method->code_length
	  && ! (vfr->flags[end] & FLAG_INSN_START))
	verify_fail_pc ("exception end not at instruction start", end);

      vfr->flags[handler] |= FLAG_BRANCH_TARGET;
    }
}

static void
check_pool_index (int index)
{
  if (index < 0 || index >= vfy_get_constants_size (vfr->current_class))
    verify_fail_pc ("constant pool index out of range", vfr->start_PC);
}

static type
check_class_constant (int index)
{
  type t;
  vfy_constants *pool;

  check_pool_index (index);
  pool = vfy_get_constants (vfr->current_class);
  if (vfy_tag (pool, index) == JV_CONSTANT_ResolvedClass)
    init_type_from_class (&t, vfy_get_pool_class (pool, index));
  else if (vfy_tag (pool, index) == JV_CONSTANT_Class)
    init_type_from_string (&t, vfy_get_pool_string (pool, index));
  else
    verify_fail_pc ("expected class constant", vfr->start_PC);      
  return t;
}

static type
check_constant (int index)
{
  type t;
  vfy_constants *pool;

  check_pool_index (index);
  pool = vfy_get_constants (vfr->current_class);
  if (vfy_tag (pool, index) == JV_CONSTANT_ResolvedString
      || vfy_tag (pool, index) == JV_CONSTANT_String)
    init_type_from_class (&t, vfy_string_type ());
  else if (vfy_tag (pool, index) == JV_CONSTANT_Integer)
    init_type_from_tag (&t, int_type);
  else if (vfy_tag (pool, index) == JV_CONSTANT_Float)
    init_type_from_tag (&t, float_type);
  else
    verify_fail_pc ("String, int, or float constant expected", vfr->start_PC);
  return t;
}

static type
check_wide_constant (int index)
{
  type t;
  vfy_constants *pool;

  check_pool_index (index);
  pool = vfy_get_constants (vfr->current_class);
  if (vfy_tag (pool, index) == JV_CONSTANT_Long)
    init_type_from_tag (&t, long_type);
  else if (vfy_tag (pool, index) == JV_CONSTANT_Double)
    init_type_from_tag (&t, double_type);
  else
    verify_fail_pc ("long or double constant expected", vfr->start_PC);
  return t;
}

/* Helper for both field and method.  These are laid out the same in
   the constant pool.  */
static type
handle_field_or_method (int index, int expected,
			vfy_string *name, vfy_string *fmtype)
{
  vfy_uint_16 class_index, name_and_type_index;
  vfy_uint_16 name_index, desc_index;
  vfy_constants *pool;

  check_pool_index (index);
  pool = vfy_get_constants (vfr->current_class);
  if (vfy_tag (pool, index) != expected)
    verify_fail_pc ("didn't see expected constant", vfr->start_PC);
  /* Once we know we have a Fieldref or Methodref we assume that it
     is correctly laid out in the constant pool.  I think the code
     in defineclass.cc guarantees this.  */
  vfy_load_indexes (pool, index, &class_index, &name_and_type_index);
  vfy_load_indexes (pool, name_and_type_index, &name_index, &desc_index);

  *name = vfy_get_pool_string (pool, name_index);
  *fmtype = vfy_get_pool_string (pool, desc_index);

  return check_class_constant (class_index);
}

/* Return field's type, compute class' type if requested.  If
   PUTFIELD is true, use the special 'putfield' semantics.  */
static type
check_field_constant (int index, type *class_type, bool putfield)
{
  vfy_string name, field_type;
  const char *typec;
  int len;
  type t;

  type ct = handle_field_or_method (index,
				    JV_CONSTANT_Fieldref,
				    &name, &field_type);
  if (class_type)
    *class_type = ct;
  typec = vfy_string_bytes (field_type);
  len = vfy_string_length (field_type);
  if (typec[0] == '[' || typec[0] == 'L')
    init_type_from_string (&t, field_type);
  else
    init_type_from_tag (&t, get_type_val_for_signature (typec[0]));

  /* We have an obscure special case here: we can use `putfield' on a
     field declared in this class, even if `this' has not yet been
     initialized.  */
  if (putfield
      && ! type_initialized (&vfr->current_state->this_type)
      && vfr->current_state->this_type.pc == SELF
      && types_equal (&vfr->current_state->this_type, &ct)
      && vfy_class_has_field (vfr->current_class, name, field_type))
    /* Note that we don't actually know whether we're going to match
       against 'this' or some other object of the same type.  So,
       here we set things up so that it doesn't matter.  This relies
       on knowing what our caller is up to.  */
    type_set_uninitialized (class_type, EITHER);

  return t;
}

static type
check_method_constant (int index, bool is_interface,
			    vfy_string *method_name,
			    vfy_string *method_signature)
{
  return handle_field_or_method (index,
				 (is_interface
				  ? JV_CONSTANT_InterfaceMethodref
				  : JV_CONSTANT_Methodref),
				 method_name, method_signature);
}

static char *
get_one_type (char *p, type *t)
{
  const char *start = p;
  vfy_jclass k;
  type_val rt;
  char v;

  int arraycount = 0;
  while (*p == '[')
    {
      ++arraycount;
      ++p;
    }

  v = *p++;

  if (v == 'L')
    {
      vfy_string name;
      while (*p != ';')
	++p;
      ++p;
      name = vfy_get_string (start, p - start);
      *t = make_type_from_string (name);
      return p;
    }

  /* Casting to jchar here is ok since we are looking at an ASCII
     character.  */
  rt = get_type_val_for_signature (v);

  if (arraycount == 0)
    {
      /* Callers of this function eventually push their arguments on
         the stack.  So, promote them here.  */
      type new_t = make_type (rt);
      vfy_promote_type (&new_t);
      *t = new_t;
      return p;
    }

  k = construct_primitive_array_type (rt);
  while (--arraycount > 0)
    k = vfy_get_array_class (k);
  *t = make_type_from_class (k);
  return p;
}

static void 
compute_argument_types (vfy_string signature, type *types)
{
  int i;
  char *p = (char *) vfy_string_bytes (signature);

  /* Skip `('.  */
  ++p;

  i = 0;
  while (*p != ')')
    p = get_one_type (p, &types[i++]);
}

static type
compute_return_type (vfy_string signature)
{
  char *p = (char *) vfy_string_bytes (signature);
  type t;
  while (*p != ')')
    ++p;
  ++p;
  get_one_type (p, &t);
  return t;
}

static void
check_return_type (type onstack)
{
  type rt = compute_return_type (vfy_get_signature (vfr->current_method));
  if (! types_compatible (&rt, &onstack))
    verify_fail ("incompatible return type");
}

/* Initialize the stack for the new method.  Returns true if this
   method is an instance initializer.  */
static bool
initialize_stack (void)
{
  int arg_count, i;
  int var = 0;
  bool is_init = vfy_strings_equal (vfy_get_method_name (vfr->current_method),
				    vfy_init_name());
  bool is_clinit = vfy_strings_equal (vfy_get_method_name (vfr->current_method),
				      vfy_clinit_name());

  if (! vfy_is_static (vfr->current_method))
    {
      type kurr = make_type_from_class (vfr->current_class);
      if (is_init)
	{
	  type_set_uninitialized (&kurr, SELF);
	  is_init = true;
	}
      else if (is_clinit)
	verify_fail ("<clinit> method must be static");
      set_variable (0, kurr);
      state_set_this_type (vfr->current_state, &kurr);
      ++var;
    }
  else
    {
      if (is_init)
	verify_fail ("<init> method must be non-static");
    }

  /* We have to handle wide arguments specially here.  */
  arg_count = vfy_count_arguments (vfy_get_signature (vfr->current_method));
  {
    type *arg_types = (type *) vfy_alloc (arg_count * sizeof (type));
    compute_argument_types (vfy_get_signature (vfr->current_method), arg_types);
    for (i = 0; i < arg_count; ++i)
      {
	set_variable (var, arg_types[i]);
	++var;
	if (type_iswide (&arg_types[i]))
	  ++var;
      }
    vfy_free (arg_types);
  }

  return is_init;
}

static void
verify_instructions_0 (void)
{
  int i;
  bool this_is_init;

  vfr->current_state = make_state (vfr->current_method->max_stack,
				   vfr->current_method->max_locals);

  vfr->PC = 0;
  vfr->start_PC = 0;

  /*  True if we are verifying an instance initializer.  */
  this_is_init = initialize_stack ();

  vfr->states = (state_list **) vfy_alloc (sizeof (state_list *)
				      * vfr->current_method->code_length);

  for (i = 0; i < vfr->current_method->code_length; ++i)
    vfr->states[i] = NULL;

  vfr->next_verify_state = NULL;

  while (true)
    {
      java_opcode opcode;

      /* If the PC was invalidated, get a new one from the work list.  */
      if (vfr->PC == NO_NEXT)
	{
	  state *new_state = pop_jump ();
	  /* If it is null, we're done.  */
	  if (new_state == NULL)
	    break;

	  vfr->PC = new_state->pc;
	  debug_print ("== State pop from pending list\n");
	  /* Set up the current state.  */
	  copy_state (vfr->current_state, new_state, 
	    vfr->current_method->max_stack, vfr->current_method->max_locals);
	}
      else
	{
	  /* We only have to do this checking in the situation where
	     control flow falls through from the previous
	     instruction.  Otherwise merging is done at the time we
	     push the branch.  */
	  if (vfr->states[vfr->PC] != NULL)
	    {
	      /* We've already visited this instruction.  So merge
	         the states together.  It is simplest, but not most
	         efficient, to just always invalidate the PC here.  */
	      merge_into (vfr->PC, vfr->current_state);
	      invalidate_pc ();
	      continue;
	    }
	}

      /* Control can't fall off the end of the bytecode.  We need to
         check this in both cases, not just the fall-through case,
         because we don't check to see whether a `jsr' appears at
         the end of the bytecode until we process a `ret'.  */
      if (vfr->PC >= vfr->current_method->code_length)
	verify_fail ("fell off end");
      vfr->flags[vfr->PC] |= FLAG_INSN_SEEN;

      /* We only have to keep saved state at branch targets.  If
         we're at a branch target and the state here hasn't been set
         yet, we set it now.  You might notice that `ret' targets
         won't necessarily have FLAG_BRANCH_TARGET set.  This
         doesn't matter, since those states will be filled in by
         merge_into.  */
      /* Note that other parts of the compiler assume that there is a
	 label with a type map at PC=0.  */
      if (vfr->states[vfr->PC] == NULL
	  && (vfr->PC == 0 || (vfr->flags[vfr->PC] & FLAG_BRANCH_TARGET) != 0))
	add_new_state (vfr->PC, vfr->current_state);

      /* Set this before handling exceptions so that debug output is
         sane.  */
      vfr->start_PC = vfr->PC;

      /* Update states for all active exception handlers.  Ordinarily
         there are not many exception handlers.  So we simply run
         through them all.  */
      for (i = 0; i < vfr->current_method->exc_count; ++i)
	{
	  int hpc, start, end, htype;
	  vfy_get_exception (vfr->exception, i, &hpc, &start, &end, &htype);
	  if (vfr->PC >= start && vfr->PC < end)
	    {
	      type handler = make_type_from_class (vfy_throwable_type ());
	      if (htype != 0)
		handler = check_class_constant (htype);
	      push_exception_jump (handler, hpc);
	    }
	}


      debug_print_state (vfr->current_state, "   ", vfr->PC, 
			 vfr->current_method->max_stack,
			 vfr->current_method->max_locals);
      opcode = (java_opcode) vfr->bytecode[vfr->PC++];
      switch (opcode)
	{
	case op_nop:
	  break;

	case op_aconst_null:
	  push_type (null_type);
	  break;

	case op_iconst_m1:
	case op_iconst_0:
	case op_iconst_1:
	case op_iconst_2:
	case op_iconst_3:
	case op_iconst_4:
	case op_iconst_5:
	  push_type (int_type);
	  break;

	case op_lconst_0:
	case op_lconst_1:
	  push_type (long_type);
	  break;

	case op_fconst_0:
	case op_fconst_1:
	case op_fconst_2:
	  push_type (float_type);
	  break;

	case op_dconst_0:
	case op_dconst_1:
	  push_type (double_type);
	  break;

	case op_bipush:
	  get_byte ();
	  push_type (int_type);
	  break;

	case op_sipush:
	  get_short ();
	  push_type (int_type);
	  break;

	case op_ldc:
	  push_type_t (check_constant (get_byte ()));
	  break;
	case op_ldc_w:
	  push_type_t (check_constant (get_ushort ()));
	  break;
	case op_ldc2_w:
	  push_type_t (check_wide_constant (get_ushort ()));
	  break;

	case op_iload:
	  push_type_t (get_variable (get_byte (), int_type));
	  break;
	case op_lload:
	  push_type_t (get_variable (get_byte (), long_type));
	  break;
	case op_fload:
	  push_type_t (get_variable (get_byte (), float_type));
	  break;
	case op_dload:
	  push_type_t (get_variable (get_byte (), double_type));
	  break;
	case op_aload:
	  push_type_t (get_variable (get_byte (), reference_type));
	  break;

	case op_iload_0:
	case op_iload_1:
	case op_iload_2:
	case op_iload_3:
	  push_type_t (get_variable (opcode - op_iload_0, int_type));
	  break;
	case op_lload_0:
	case op_lload_1:
	case op_lload_2:
	case op_lload_3:
	  push_type_t (get_variable (opcode - op_lload_0, long_type));
	  break;
	case op_fload_0:
	case op_fload_1:
	case op_fload_2:
	case op_fload_3:
	  push_type_t (get_variable (opcode - op_fload_0, float_type));
	  break;
	case op_dload_0:
	case op_dload_1:
	case op_dload_2:
	case op_dload_3:
	  push_type_t (get_variable (opcode - op_dload_0, double_type));
	  break;
	case op_aload_0:
	case op_aload_1:
	case op_aload_2:
	case op_aload_3:
	  push_type_t (get_variable (opcode - op_aload_0, reference_type));
	  break;
	case op_iaload:
	  pop_type (int_type);
	  push_type_t (require_array_type (pop_init_ref (reference_type),
					 int_type));
	  break;
	case op_laload:
	  pop_type (int_type);
	  push_type_t (require_array_type (pop_init_ref (reference_type),
					 long_type));
	  break;
	case op_faload:
	  pop_type (int_type);
	  push_type_t (require_array_type (pop_init_ref (reference_type),
					 float_type));
	  break;
	case op_daload:
	  pop_type (int_type);
	  push_type_t (require_array_type (pop_init_ref (reference_type),
					 double_type));
	  break;
	case op_aaload:
	  pop_type (int_type);
	  push_type_t (require_array_type (pop_init_ref (reference_type),
					 reference_type));
	  break;
	case op_baload:
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), byte_type);
	  push_type (int_type);
	  break;
	case op_caload:
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), char_type);
	  push_type (int_type);
	  break;
	case op_saload:
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), short_type);
	  push_type (int_type);
	  break;
	case op_istore:
	  set_variable (get_byte (), pop_type (int_type));
	  break;
	case op_lstore:
	  set_variable (get_byte (), pop_type (long_type));
	  break;
	case op_fstore:
	  set_variable (get_byte (), pop_type (float_type));
	  break;
	case op_dstore:
	  set_variable (get_byte (), pop_type (double_type));
	  break;
	case op_astore:
	  set_variable (get_byte (), pop_ref_or_return ());
	  break;
	case op_istore_0:
	case op_istore_1:
	case op_istore_2:
	case op_istore_3:
	  set_variable (opcode - op_istore_0, pop_type (int_type));
	  break;
	case op_lstore_0:
	case op_lstore_1:
	case op_lstore_2:
	case op_lstore_3:
	  set_variable (opcode - op_lstore_0, pop_type (long_type));
	  break;
	case op_fstore_0:
	case op_fstore_1:
	case op_fstore_2:
	case op_fstore_3:
	  set_variable (opcode - op_fstore_0, pop_type (float_type));
	  break;
	case op_dstore_0:
	case op_dstore_1:
	case op_dstore_2:
	case op_dstore_3:
	  set_variable (opcode - op_dstore_0, pop_type (double_type));
	  break;
	case op_astore_0:
	case op_astore_1:
	case op_astore_2:
	case op_astore_3:
	  set_variable (opcode - op_astore_0, pop_ref_or_return ());
	  break;
	case op_iastore:
	  pop_type (int_type);
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), int_type);
	  break;
	case op_lastore:
	  pop_type (long_type);
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), long_type);
	  break;
	case op_fastore:
	  pop_type (float_type);
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), float_type);
	  break;
	case op_dastore:
	  pop_type (double_type);
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), double_type);
	  break;
	case op_aastore:
	  pop_type (reference_type);
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), reference_type);
	  break;
	case op_bastore:
	  pop_type (int_type);
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), byte_type);
	  break;
	case op_castore:
	  pop_type (int_type);
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), char_type);
	  break;
	case op_sastore:
	  pop_type (int_type);
	  pop_type (int_type);
	  require_array_type (pop_init_ref (reference_type), short_type);
	  break;
	case op_pop:
	  pop32 ();
	  break;
	case op_pop2:
	  {
	    type t = pop_raw ();
	    if (! type_iswide (&t))
	      pop32 ();
	  }
	  break;
	case op_dup:
	  {
	    type t = pop32 ();
	    push_type_t (t);
	    push_type_t (t);
	  }
	  break;
	case op_dup_x1:
	  {
	    type t1 = pop32 ();
	    type t2 = pop32 ();
	    push_type_t (t1);
	    push_type_t (t2);
	    push_type_t (t1);
	  }
	  break;
	case op_dup_x2:
	  {
	    type t1 = pop32 ();
	    type t2 = pop_raw ();
	    if (! type_iswide (&t2))
	      {
		type t3 = pop32 ();
		push_type_t (t1);
		push_type_t (t3);
	      }
	    else
	      push_type_t (t1);
	    push_type_t (t2);
	    push_type_t (t1);
	  }
	  break;
	case op_dup2:
	  {
	    type t = pop_raw ();
	    if (! type_iswide (&t))
	      {
		type t2 = pop32 ();
		push_type_t (t2);
		push_type_t (t);
		push_type_t (t2);
	      }
	    else
	      push_type_t (t);
	    push_type_t (t);
	  }
	  break;
	case op_dup2_x1:
	  {
	    type t1 = pop_raw ();
	    type t2 = pop32 ();
	    if (! type_iswide (&t1))
	      {
		type t3 = pop32 ();
		push_type_t (t2);
		push_type_t (t1);
		push_type_t (t3);
	      }
	    else
	      push_type_t (t1);
	    push_type_t (t2);
	    push_type_t (t1);
	  }
	  break;
	case op_dup2_x2:
	  {
	    type t1 = pop_raw ();
	    if (type_iswide (&t1))
	      {
		type t2 = pop_raw ();
		if (type_iswide (&t2))
		  {
		    push_type_t (t1);
		    push_type_t (t2);
		  }
		else
		  {
		    type t3 = pop32 ();
		    push_type_t (t1);
		    push_type_t (t3);
		    push_type_t (t2);
		  }
		push_type_t (t1);
	      }
	    else
	      {
		type t2 = pop32 ();
		type t3 = pop_raw ();
		if (type_iswide (&t3))
		  {
		    push_type_t (t2);
		    push_type_t (t1);
		  }
		else
		  {
		    type t4 = pop32 ();
		    push_type_t (t2);
		    push_type_t (t1);
		    push_type_t (t4);
		  }
		push_type_t (t3);
		push_type_t (t2);
		push_type_t (t1);
	      }
	  }
	  break;
	case op_swap:
	  {
	    type t1 = pop32 ();
	    type t2 = pop32 ();
	    push_type_t (t1);
	    push_type_t (t2);
	  }
	  break;
	case op_iadd:
	case op_isub:
	case op_imul:
	case op_idiv:
	case op_irem:
	case op_ishl:
	case op_ishr:
	case op_iushr:
	case op_iand:
	case op_ior:
	case op_ixor:
	  pop_type (int_type);
	  push_type_t (pop_type (int_type));
	  break;
	case op_ladd:
	case op_lsub:
	case op_lmul:
	case op_ldiv:
	case op_lrem:
	case op_land:
	case op_lor:
	case op_lxor:
	  pop_type (long_type);
	  push_type_t (pop_type (long_type));
	  break;
	case op_lshl:
	case op_lshr:
	case op_lushr:
	  pop_type (int_type);
	  push_type_t (pop_type (long_type));
	  break;
	case op_fadd:
	case op_fsub:
	case op_fmul:
	case op_fdiv:
	case op_frem:
	  pop_type (float_type);
	  push_type_t (pop_type (float_type));
	  break;
	case op_dadd:
	case op_dsub:
	case op_dmul:
	case op_ddiv:
	case op_drem:
	  pop_type (double_type);
	  push_type_t (pop_type (double_type));
	  break;
	case op_ineg:
	case op_i2b:
	case op_i2c:
	case op_i2s:
	  push_type_t (pop_type (int_type));
	  break;
	case op_lneg:
	  push_type_t (pop_type (long_type));
	  break;
	case op_fneg:
	  push_type_t (pop_type (float_type));
	  break;
	case op_dneg:
	  push_type_t (pop_type (double_type));
	  break;
	case op_iinc:
	  get_variable (get_byte (), int_type);
	  get_byte ();
	  break;
	case op_i2l:
	  pop_type (int_type);
	  push_type (long_type);
	  break;
	case op_i2f:
	  pop_type (int_type);
	  push_type (float_type);
	  break;
	case op_i2d:
	  pop_type (int_type);
	  push_type (double_type);
	  break;
	case op_l2i:
	  pop_type (long_type);
	  push_type (int_type);
	  break;
	case op_l2f:
	  pop_type (long_type);
	  push_type (float_type);
	  break;
	case op_l2d:
	  pop_type (long_type);
	  push_type (double_type);
	  break;
	case op_f2i:
	  pop_type (float_type);
	  push_type (int_type);
	  break;
	case op_f2l:
	  pop_type (float_type);
	  push_type (long_type);
	  break;
	case op_f2d:
	  pop_type (float_type);
	  push_type (double_type);
	  break;
	case op_d2i:
	  pop_type (double_type);
	  push_type (int_type);
	  break;
	case op_d2l:
	  pop_type (double_type);
	  push_type (long_type);
	  break;
	case op_d2f:
	  pop_type (double_type);
	  push_type (float_type);
	  break;
	case op_lcmp:
	  pop_type (long_type);
	  pop_type (long_type);
	  push_type (int_type);
	  break;
	case op_fcmpl:
	case op_fcmpg:
	  pop_type (float_type);
	  pop_type (float_type);
	  push_type (int_type);
	  break;
	case op_dcmpl:
	case op_dcmpg:
	  pop_type (double_type);
	  pop_type (double_type);
	  push_type (int_type);
	  break;
	case op_ifeq:
	case op_ifne:
	case op_iflt:
	case op_ifge:
	case op_ifgt:
	case op_ifle:
	  pop_type (int_type);
	  push_jump (get_short ());
	  break;
	case op_if_icmpeq:
	case op_if_icmpne:
	case op_if_icmplt:
	case op_if_icmpge:
	case op_if_icmpgt:
	case op_if_icmple:
	  pop_type (int_type);
	  pop_type (int_type);
	  push_jump (get_short ());
	  break;
	case op_if_acmpeq:
	case op_if_acmpne:
	  pop_type (reference_type);
	  pop_type (reference_type);
	  push_jump (get_short ());
	  break;
	case op_goto:
	  push_jump (get_short ());
	  invalidate_pc ();
	  break;
	case op_jsr:
	  handle_jsr_insn (get_short ());
	  break;
	case op_ret:
	  handle_ret_insn (get_byte ());
	  break;
	case op_tableswitch:
	  {
	    int i;
	    jint low, high;
	    pop_type (int_type);
	    skip_padding ();
	    push_jump (get_int ());
	    low = get_int ();
	    high = get_int ();
	    /* Already checked LOW -vs- HIGH.  */
	    for (i = low; i <= high; ++i)
	      push_jump (get_int ());
	    invalidate_pc ();
	  }
	  break;

	case op_lookupswitch:
	  {
	    int i;
	    jint npairs, lastkey;

	    pop_type (int_type);
	    skip_padding ();
	    push_jump (get_int ());
	    npairs = get_int ();
	    /* Already checked NPAIRS >= 0.  */
	    lastkey = 0;
	    for (i = 0; i < npairs; ++i)
	      {
		jint key = get_int ();
		if (i > 0 && key <= lastkey)
		  verify_fail_pc ("lookupswitch pairs unsorted", vfr->start_PC);
		lastkey = key;
		push_jump (get_int ());
	      }
	    invalidate_pc ();
	  }
	  break;
	case op_ireturn:
	  check_return_type (pop_type (int_type));
	  invalidate_pc ();
	  break;
	case op_lreturn:
	  check_return_type (pop_type (long_type));
	  invalidate_pc ();
	  break;
	case op_freturn:
	  check_return_type (pop_type (float_type));
	  invalidate_pc ();
	  break;
	case op_dreturn:
	  check_return_type (pop_type (double_type));
	  invalidate_pc ();
	  break;
	case op_areturn:
	  check_return_type (pop_init_ref (reference_type));
	  invalidate_pc ();
	  break;
	case op_return:
	  /* We only need to check this when the return type is
	     void, because all instance initializers return void.  */
	  if (this_is_init)
	    state_check_this_initialized (vfr->current_state);
	  check_return_type (make_type (void_type));
	  invalidate_pc ();
	  break;
	case op_getstatic:
	  push_type_t (check_field_constant (get_ushort (), NULL, false));
	  break;
	case op_putstatic:
	  pop_type_t (check_field_constant (get_ushort (), NULL, false));
	  break;
	case op_getfield:
	  {
	    type klass;
	    type field = check_field_constant (get_ushort (), &klass, false);
	    pop_type_t (klass);
	    push_type_t (field);
	  }
	  break;
	case op_putfield:
	  {
	    type klass;
	    type field = check_field_constant (get_ushort (), &klass, true);
	    pop_type_t (field);
	    pop_type_t (klass);
	  }
	  break;

	case op_invokevirtual:
	case op_invokespecial:
	case op_invokestatic:
	case op_invokeinterface:
	  {
	    vfy_string method_name, method_signature;
	    const char *namec;
	    int i, arg_count;
	    type rt;
	    bool is_init = false;

	    type class_type
	      = check_method_constant (get_ushort (),
				       opcode == op_invokeinterface,
				       &method_name,
				       &method_signature);
	    /* NARGS is only used when we're processing
	       invokeinterface.  It is simplest for us to compute it
	       here and then verify it later.  */
	    int nargs = 0;
	    if (opcode == op_invokeinterface)
	      {
		nargs = get_byte ();
		if (get_byte () != 0)
		  verify_fail ("invokeinterface dummy byte is wrong");
	      }

	    namec = vfy_string_bytes (method_name);

	    if (vfy_strings_equal (method_name, vfy_init_name()))
	      {
		is_init = true;
		if (opcode != op_invokespecial)
		  verify_fail ("can't invoke <init>");
	      }
	    else if (namec[0] == '<')
	      verify_fail ("can't invoke method starting with `<'");

	    arg_count = vfy_count_arguments (method_signature);
            {
	      /* Pop arguments and check types.  */
	      type *arg_types = (type *) vfy_alloc (arg_count * sizeof (type));

	      compute_argument_types (method_signature, arg_types);
	      for (i = arg_count - 1; i >= 0; --i)
		{
		  /* This is only used for verifying the byte for
		     invokeinterface.  */
		  nargs -= type_depth (&arg_types[i]);
		  pop_init_ref_t (arg_types[i]);
		}

	      vfy_free (arg_types);
	    }

	    if (opcode == op_invokeinterface
		&& nargs != 1)
	      verify_fail ("wrong argument count for invokeinterface");

	    if (opcode != op_invokestatic)
	      {
	        type raw;
		type t = class_type;
		if (is_init)
		  {
		    /* In this case the PC doesn't matter.  */
		    type_set_uninitialized (&t, UNINIT);
		    /* FIXME: check to make sure that the <init>
		       call is to the right class.
		       It must either be super or an exact class
		       match.  */
		  }
		raw = pop_raw ();
		if (! types_compatible (&t, &raw))
		  verify_fail ("incompatible type on stack");

		if (is_init)		  
		  state_set_initialized (vfr->current_state, 
		    type_get_pc (&raw), vfr->current_method->max_locals);
	      }

	    rt = compute_return_type (method_signature);
	    if (! type_isvoid (&rt))
	      push_type_t (rt);
	  }
	  break;

	case op_new:
	  {
	    type t = check_class_constant (get_ushort ());
	    if (type_isarray (&t) || type_isinterface (&t)
		|| type_isabstract (&t))
	      verify_fail ("type is array, interface, or abstract");
	    type_set_uninitialized (&t, vfr->start_PC);
	    push_type_t (t);
	  }
	  break;

	case op_newarray:
	  {
	    int atype = get_byte ();
	    type t;
	    /* We intentionally have chosen constants to make this
	       valid.  */
	    if (atype < boolean_type || atype > long_type)
	      verify_fail_pc ("type not primitive", vfr->start_PC);
	    pop_type (int_type);
	    init_type_from_class (&t, construct_primitive_array_type (atype));
	    push_type_t (t);
	  }
	  break;
	case op_anewarray:
	  {
	    type t;
	    pop_type (int_type);
	    t = check_class_constant (get_ushort ());
	    push_type_t (type_to_array (&t));
	  }
	  break;
	case op_arraylength:
	  {
	    type t = pop_init_ref (reference_type);
	    if (! type_isarray (&t) && ! type_isnull (&t))
	      verify_fail ("array type expected");
	    push_type (int_type);
	  }
	  break;
	case op_athrow:
	  pop_type_t (make_type_from_class (vfy_throwable_type ()));
	  invalidate_pc ();
	  break;
	case op_checkcast:
	  pop_init_ref (reference_type);
	  push_type_t (check_class_constant (get_ushort ()));
	  break;
	case op_instanceof:
	  pop_init_ref (reference_type);
	  check_class_constant (get_ushort ());
	  push_type (int_type);
	  break;
	case op_monitorenter:
	  pop_init_ref (reference_type);
	  break;
	case op_monitorexit:
	  pop_init_ref (reference_type);
	  break;
	case op_wide:
	  {
	    switch (get_byte ())
	      {
	      case op_iload:
		push_type_t (get_variable (get_ushort (), int_type));
		break;
	      case op_lload:
		push_type_t (get_variable (get_ushort (), long_type));
		break;
	      case op_fload:
		push_type_t (get_variable (get_ushort (), float_type));
		break;
	      case op_dload:
		push_type_t (get_variable (get_ushort (), double_type));
		break;
	      case op_aload:
		push_type_t (get_variable (get_ushort (), reference_type));
		break;
	      case op_istore:
		set_variable (get_ushort (), pop_type (int_type));
		break;
	      case op_lstore:
		set_variable (get_ushort (), pop_type (long_type));
		break;
	      case op_fstore:
		set_variable (get_ushort (), pop_type (float_type));
		break;
	      case op_dstore:
		set_variable (get_ushort (), pop_type (double_type));
		break;
	      case op_astore:
		set_variable (get_ushort (), pop_init_ref (reference_type));
		break;
	      case op_ret:
		handle_ret_insn (get_short ());
		break;
	      case op_iinc:
		get_variable (get_ushort (), int_type);
		get_short ();
		break;
	      default:
		verify_fail_pc ("unrecognized wide instruction", vfr->start_PC);
	      }
	  }
	  break;
	case op_multianewarray:
	  {
	    int i;
	    type atype = check_class_constant (get_ushort ());
	    int dim = get_byte ();
	    if (dim < 1)
	      verify_fail_pc ("too few dimensions to multianewarray", vfr->start_PC);
            type_verify_dimensions (&atype, dim);
	    for (i = 0; i < dim; ++i)
	      pop_type (int_type);
	    push_type_t (atype);
	  }
	  break;
	case op_ifnull:
	case op_ifnonnull:
	  pop_type (reference_type);
	  push_jump (get_short ());
	  break;
	case op_goto_w:
	  push_jump (get_int ());
	  invalidate_pc ();
	  break;
	case op_jsr_w:
	  handle_jsr_insn (get_int ());
	  break;

	default:
	  /* Unrecognized opcode.  */
	  verify_fail_pc ("unrecognized instruction in verify_instructions_0",
		       vfr->start_PC);
	}
    }
}

/* This turns a `type' into something suitable for use by the type map
   in the other parts of the compiler.  In particular, reference types
   are mapped to Object, primitive types are unchanged, and other
   types are mapped using special functions declared in verify.h.  */
static vfy_jclass
collapse_type (type *t)
{
  switch (t->key)
    {
    case void_type:
    case boolean_type:
    case char_type:
    case float_type:
    case double_type:
    case byte_type:
    case short_type:
    case int_type:
    case long_type:
      return vfy_get_primitive_type (t->key);

    case unsuitable_type:
    case continuation_type:
      return vfy_unsuitable_type ();

    case return_address_type:
      return vfy_return_address_type ();

    case null_type:
      return vfy_null_type ();

    case reference_type:
    case uninitialized_reference_type:
      return vfy_object_type ();
    }

  abort ();
}

static void
verify_instructions (void)
{
  int i;

  branch_prepass ();
  verify_instructions_0 ();

  /* Now tell the rest of the compiler about the types we've found.  */
  for (i = 0; i < vfr->current_method->code_length; ++i)
    {
      int j, slot;
      struct state *curr;

      if ((vfr->flags[i] & FLAG_INSN_SEEN) != 0)
	vfy_note_instruction_seen (i);

      if (! vfr->states[i])
	continue;

      curr = vfr->states[i]->val;
      vfy_note_stack_depth (vfr->current_method, i, curr->stackdepth);

      /* Tell the compiler about each local variable.  */
      for (j = 0; j < vfr->current_method->max_locals; ++j)
	vfy_note_local_type (vfr->current_method, i, j,
			     collapse_type (&curr->locals[j]));
      /* Tell the compiler about each stack slot.  */
      for (slot = j = 0; j < curr->stacktop; ++j, ++slot)
	{
	  vfy_note_stack_type (vfr->current_method, i, slot,
			       collapse_type (&curr->stack[j]));
	  if (type_iswide (&curr->stack[j]))
	    {
	      ++slot;
	      vfy_note_stack_type (vfr->current_method, i, slot,
				   vfy_unsuitable_type ());
	    }
	}
      if (slot != curr->stackdepth)
	abort ();
    }
}

static void
make_verifier_context (vfy_method *m)
{
  vfr = (verifier_context *) vfy_alloc (sizeof (struct verifier_context));

  vfr->current_method = m;
  vfr->bytecode = vfy_get_bytecode (m);
  vfr->exception = vfy_get_exceptions (m);
  vfr->current_class = m->defining_class;

  vfr->states = NULL;
  vfr->flags = NULL;
  vfr->utf8_list = NULL;
  vfr->isect_list = NULL;
}

static void
free_verifier_context (void)
{
  vfy_string_list *utf8_list;
  ref_intersection *isect_list;

  if (vfr->flags)
    vfy_free (vfr->flags);

  utf8_list = vfr->utf8_list;
  while (utf8_list != NULL)
    {
      vfy_string_list *n = utf8_list->next;
      vfy_free (utf8_list);
      utf8_list = n;
    }

  isect_list = vfr->isect_list;
  while (isect_list != NULL)
    {
      ref_intersection *next = isect_list->alloc_next;
      vfy_free (isect_list);
      isect_list = next;
    }

  if (vfr->states != NULL)
    {
      int i;
      for (i = 0; i < vfr->current_method->code_length; ++i)
	{
	  state_list *iter = vfr->states[i];
	  while (iter != NULL)
	    {
	      state_list *next = iter->next;
	      free_state (iter->val);
	      vfy_free (iter->val);
	      vfy_free (iter);
	      iter = next;
	    }
	}
      vfy_free (vfr->states);
    }
  
  vfy_free (vfr);
}

int
verify_method (vfy_method *meth)
{
  debug_print ("verify_method (%s) %i\n", vfy_string_bytes (meth->name),
	       meth->code_length);
  
  if (vfr != NULL)
    verify_fail ("verifier re-entered");

  make_verifier_context (meth);
  verify_instructions ();
  free_verifier_context ();
  vfr = NULL;

  return 1;
}
