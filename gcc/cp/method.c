/* Handle the hair of processing (but not expanding) inline functions.
   Also manage function and variable name overloading.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GNU CC.
   
GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Handle method declarations.  */
#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "obstack.h"
#include "rtl.h"
#include "expr.h"
#include "output.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"

/* Various flags to control the mangling process.  */

enum mangling_flags
{
  /* No flags.  */
  mf_none = 0,
  /* The thing we are presently mangling is part of a template type,
     rather than a fully instantiated type.  Therefore, we may see
     complex expressions where we would normally expect to see a
     simple integer constant.  */
  mf_maybe_uninstantiated = 1,
  /* When mangling a numeric value, use the form `_XX_' (instead of
     just `XX') if the value has more than one digit.  */
  mf_use_underscores_around_value = 2,
};

typedef enum mangling_flags mangling_flags;

/* TREE_LIST of the current inline functions that need to be
   processed.  */
struct pending_inline *pending_inlines;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Obstack where we build text strings for overloading, etc.  */
static struct obstack scratch_obstack;
static char *scratch_firstobj;

static void icat PARAMS ((HOST_WIDE_INT));
static void dicat PARAMS ((HOST_WIDE_INT, HOST_WIDE_INT));
static int old_backref_index PARAMS ((tree));
static int flush_repeats PARAMS ((int, tree));
static void build_overload_identifier PARAMS ((tree));
static void build_overload_nested_name PARAMS ((tree));
static void mangle_expression PARAMS ((tree));
static void build_overload_int PARAMS ((tree, mangling_flags));
static void build_overload_identifier PARAMS ((tree));
static void build_qualified_name PARAMS ((tree));
static void build_overload_value PARAMS ((tree, tree, mangling_flags));
static void issue_nrepeats PARAMS ((int, tree));
static char *build_mangled_name PARAMS ((tree,int,int));
static void process_modifiers PARAMS ((tree));
static void process_overload_item PARAMS ((tree,int));
static void do_build_assign_ref PARAMS ((tree));
static void do_build_copy_constructor PARAMS ((tree));
static void build_template_template_parm_names PARAMS ((tree));
static void build_template_parm_names PARAMS ((tree, tree));
static void build_underscore_int PARAMS ((int));
static void start_squangling PARAMS ((void));
static void end_squangling PARAMS ((void));
static int check_ktype PARAMS ((tree, int));
static int issue_ktype PARAMS ((tree));
static void build_overload_scope_ref PARAMS ((tree));
static void build_mangled_template_parm_index PARAMS ((const char *, tree));
#if HOST_BITS_PER_WIDE_INT >= 64
static void build_mangled_C9x_name PARAMS ((int));
#endif
static int is_back_referenceable_type PARAMS ((tree));
static int check_btype PARAMS ((tree));
static void build_mangled_name_for_type PARAMS ((tree));
static void build_mangled_name_for_type_with_Gcode PARAMS ((tree, int));

# define OB_INIT() (scratch_firstobj ? (obstack_free (&scratch_obstack, scratch_firstobj), 0) : 0)
# define OB_PUTC(C) (obstack_1grow (&scratch_obstack, (C)))
# define OB_PUTC2(C1,C2)	\
  (obstack_1grow (&scratch_obstack, (C1)), obstack_1grow (&scratch_obstack, (C2)))
# define OB_PUTS(S) (obstack_grow (&scratch_obstack, (S), sizeof (S) - 1))
# define OB_PUTID(ID)  \
  (obstack_grow (&scratch_obstack, IDENTIFIER_POINTER (ID),	\
		 IDENTIFIER_LENGTH (ID)))
# define OB_PUTCP(S) (obstack_grow (&scratch_obstack, (S), strlen (S)))
# define OB_FINISH() (obstack_1grow (&scratch_obstack, '\0'))
# define OB_LAST() (obstack_next_free (&scratch_obstack)[-1])

/* type tables for K and B type compression */
static varray_type btypelist;
static varray_type ktypelist;

/* number of each type seen */
static size_t maxbtype;
static size_t maxktype;

/* Array of types seen so far in top-level call to `build_mangled_name'.
   Allocated and deallocated by caller.  */
static varray_type typevec;

/* Number of types interned by `build_mangled_name' so far.  */
static size_t maxtype;

/* Called once to initialize method.c.  */

void
init_method ()
{
  gcc_obstack_init (&scratch_obstack);
  scratch_firstobj = (char *)obstack_alloc (&scratch_obstack, 0);
  ggc_add_tree_varray_root (&btypelist, 1);
  ggc_add_tree_varray_root (&ktypelist, 1);
  ggc_add_tree_varray_root (&typevec, 1);
}

/* This must be large enough to hold any printed integer or floating-point
   value.  */
static char digit_buffer[128];


/* Here is where overload code starts.  */

/* Nonzero if we should not try folding parameter types.  */
static int nofold;

/* Nonzero if an underscore is required before adding a digit to the
   mangled name currently being built.  */
static int numeric_output_need_bar;

static inline void
start_squangling ()
{
  if (flag_do_squangling)
    {
      nofold = 0;
      maxbtype = 0;
      maxktype = 0;
      VARRAY_TREE_INIT (btypelist, 50, "btypelist");
      VARRAY_TREE_INIT (ktypelist, 50, "ktypelist");
    }
}

static inline void
end_squangling ()
{
  if (flag_do_squangling)
    {
      VARRAY_FREE (ktypelist);
      VARRAY_FREE (btypelist);
      maxbtype = 0;
      maxktype = 0;
    }
}

/* Code to concatenate an asciified integer to a string.  */

static inline void
icat (i)
     HOST_WIDE_INT i;
{
  unsigned HOST_WIDE_INT ui;

  /* Handle this case first, to go really quickly.  For many common values,
     the result of ui/10 below is 1.  */
  if (i == 1)
    {
      OB_PUTC ('1');
      return;
    }

  if (i >= 0)
    ui = i;
  else
    {
      OB_PUTC ('m');
      ui = -i;
    }

  if (ui >= 10)
    icat (ui / 10);

  OB_PUTC ('0' + (ui % 10));
}

static void
dicat (lo, hi)
     HOST_WIDE_INT lo, hi;
{
  unsigned HOST_WIDE_INT ulo, uhi, qlo, qhi;

  if (hi >= 0)
    {
      uhi = hi;
      ulo = lo;
    }
  else
    {
      uhi = (lo == 0 ? -hi : -hi-1);
      ulo = -lo;
    }
  if (uhi == 0
      && ulo < ((unsigned HOST_WIDE_INT)1 << (HOST_BITS_PER_WIDE_INT - 1)))
    {
      icat (ulo);
      return;
    }
  /* Divide 2^HOST_WIDE_INT*uhi+ulo by 10. */
  qhi = uhi / 10;
  uhi = uhi % 10;
  qlo = uhi * (((unsigned HOST_WIDE_INT)1 << (HOST_BITS_PER_WIDE_INT - 1)) / 5);
  qlo += ulo / 10;
  ulo = ulo % 10;
  ulo += uhi * (((unsigned HOST_WIDE_INT)1 << (HOST_BITS_PER_WIDE_INT - 1)) % 5)
	 * 2;
  qlo += ulo / 10;
  ulo = ulo % 10;
  /* Quotient is 2^HOST_WIDE_INT*qhi+qlo, remainder is ulo. */
  dicat (qlo, qhi);
  OB_PUTC ('0' + ulo);
}

/* Returns the index of TYPE in the typevec, or -1 if it's not there.  */

static inline int
old_backref_index (type)
     tree type;
{
  size_t tindex;

  if (! is_back_referenceable_type (type))
    return -1;

  /* The entry for this parm is at maxtype-1, so don't look there for
     something to repeat.  */
  for (tindex = 0; tindex < maxtype - 1; ++tindex)
    if (same_type_p (VARRAY_TREE (typevec, tindex), type))
      break;

  if (tindex == maxtype - 1)
    return -1;

  return tindex;
}

/* Old mangling style:  If TYPE has already been used in the parameter list,
   emit a backward reference and return non-zero; otherwise, return 0.

   NREPEATS is the number of repeats we've recorded of this type, or 0 if
   this is the first time we've seen it and we're just looking to see if
   it had been used before.  */

static inline int
flush_repeats (nrepeats, type)
     int nrepeats;
     tree type;
{
  int tindex = old_backref_index (type);

  if (tindex == -1)
    {
      my_friendly_assert (nrepeats == 0, 990316);
      return 0;
    }

  if (nrepeats > 1)
    {
      OB_PUTC ('N');
      icat (nrepeats);
      if (nrepeats > 9)
	OB_PUTC ('_');
    }
  else
    OB_PUTC ('T');
  icat (tindex);
  if (tindex > 9)
    OB_PUTC ('_');

  return 1;
}

/* Returns nonzero iff this is a type to which we will want to make
   back-references (using the `B' code).  */

static int
is_back_referenceable_type (type)
     tree type;
{
  /* For some reason, the Java folks don't want back refs on these.  */
  if (TYPE_FOR_JAVA (type))
    return 0;

  switch (TREE_CODE (type)) 
    {
    case BOOLEAN_TYPE:
      if (!flag_do_squangling)
	/* Even though the mangling of this is just `b', we did
	   historically generate back-references for it.  */
	return 1;
      /* Fall through.  */
      
    case INTEGER_TYPE:
    case REAL_TYPE:
    case VOID_TYPE:
      /* These types have single-character manglings, so there's no
	 point in generating back-references.  */
      return 0;         

    case TEMPLATE_TYPE_PARM:
      /* It would be a bit complex to demangle signatures correctly if
	 we generated back-references to these, and the manglings of
	 type parameters are short.  */
      return 0;

    default:
      return 1;
    }
}

/* Issue the squangling code indicating NREPEATS repetitions of TYPE,
   which was the last parameter type output.  */

static void
issue_nrepeats (nrepeats, type)
     int nrepeats;
     tree type;
{
  if (nrepeats == 1 && !is_back_referenceable_type (type))
    /* For types whose manglings are short, don't bother using the
       repetition code if there's only one repetition, since the
       repetition code will be about as long as the ordinary mangling.  */ 
    build_mangled_name_for_type (type);
  else
    {
      OB_PUTC ('n');
      icat (nrepeats);
      if (nrepeats > 9)
	OB_PUTC ('_');
    }
}

/* Check to see if a tree node has been entered into the Kcode typelist.
   If not, add it.  Returns -1 if it isn't found, otherwise returns the
   index.  */

static int
check_ktype (node, add)
     tree node;
     int add;
{
  size_t x;
  tree localnode = node;

  if (ktypelist == NULL)
    return -1;

  if (TREE_CODE (node) == TYPE_DECL)
    localnode = TREE_TYPE (node);

  for (x = 0; x < maxktype; x++)
    {
      if (same_type_p (localnode, VARRAY_TREE (ktypelist, x)))
        return x;
    }
  /* Didn't find it, so add it here.  */
  if (add)
    {
      if (VARRAY_SIZE (ktypelist) <= maxktype)
	VARRAY_GROW (ktypelist, 
		     VARRAY_SIZE (ktypelist) * 3 / 2);
      VARRAY_TREE (ktypelist, maxktype) = localnode;
      maxktype++;
    }
  return -1;
}


static inline int
issue_ktype (decl)
     tree decl;
{
  int kindex;
  kindex = check_ktype (decl, FALSE);
  if (kindex != -1)
    {
      OB_PUTC ('K');
      icat (kindex);
      if (kindex > 9)
        OB_PUTC ('_');
      return TRUE;
    }
  return FALSE;
}
  
/* Build a representation for DECL, which may be an entity not at
   global scope.  If so, a marker indicating that the name is
   qualified has already been output, but the qualifying context has
   not.  */

static void
build_overload_nested_name (decl)
     tree decl;
{
  tree context;

  if (ktypelist && issue_ktype (decl))
      return;

  if (decl == global_namespace)
    return;

  context = CP_DECL_CONTEXT (decl);

  /* try to issue a K type, and if we can't continue the normal path */
  if (!(ktypelist && issue_ktype (context)))
  {
    /* For a template type parameter, we want to output an 'Xn'
       rather than 'T' or some such. */
    if (TREE_CODE (context) == TEMPLATE_TYPE_PARM
        || TREE_CODE (context) == TEMPLATE_TEMPLATE_PARM)
      build_mangled_name_for_type (context);
    else
    {
      if (TREE_CODE_CLASS (TREE_CODE (context)) == 't')
        context = TYPE_NAME (context);
      build_overload_nested_name (context);
    }
  }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      static int static_labelno;

      tree name = DECL_ASSEMBLER_NAME (decl);
      char *label;

      ASM_FORMAT_PRIVATE_NAME (label, IDENTIFIER_POINTER (name), static_labelno);
      static_labelno++;

      if (numeric_output_need_bar)
	OB_PUTC ('_');
      icat (strlen (label));
      OB_PUTCP (label);
      numeric_output_need_bar = 1;
    }
  else if (TREE_CODE (decl) == NAMESPACE_DECL)
    build_overload_identifier (DECL_NAME (decl));
  else				/* TYPE_DECL */
    build_overload_identifier (decl);
}

/* Output the decimal representation of I.  If I > 9, the decimal
   representation is preceeded and followed by an underscore.  */

static void
build_underscore_int (i)
     int i;
{
  if (i > 9)
    OB_PUTC ('_');
  icat (i);
  if (i > 9)
    OB_PUTC ('_');
}

static void
build_overload_scope_ref (value)
     tree value;
{
  OB_PUTC2 ('Q', '2');
  numeric_output_need_bar = 0;
  build_mangled_name_for_type (TREE_OPERAND (value, 0));
  build_overload_identifier (TREE_OPERAND (value, 1));
}

/* VALUE is a complex expression.  Produce an appropriate mangling.
   (We are forced to mangle complex expressions when dealing with
   templates, and an expression involving template parameters appears
   in the type of a function parameter.)  */

static void
mangle_expression (value)
     tree value;
{
  if (TREE_CODE (value) == SCOPE_REF)
    {
      build_overload_scope_ref (value);
      return;
    }

  OB_PUTC ('E');
  numeric_output_need_bar = 0;

  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (TREE_CODE (value))))
    {
      int i;
      int operands = tree_code_length[(int) TREE_CODE (value)];
      tree id;
      const char *name;

      id = ansi_opname [(int) TREE_CODE (value)];
      my_friendly_assert (id != NULL_TREE, 0);
      name = IDENTIFIER_POINTER (id);
      if (name[0] != '_' || name[1] != '_')
	/* On some erroneous inputs, we can get here with VALUE a
	   LOOKUP_EXPR.  In that case, the NAME will be the
	   identifier for "<invalid operator>".  We must survive
	   this routine in order to issue a sensible error
	   message, so we fall through to the case below.  */
	goto bad_value;

      for (i = 0; i < operands; ++i)
	{
	  tree operand;
	  enum tree_code tc;

	  /* We just outputted either the `E' or the name of the
	     operator.  */
	  numeric_output_need_bar = 0;

	  if (i != 0)
	    /* Skip the leading underscores.  */
	    OB_PUTCP (name + 2);

	  operand = TREE_OPERAND (value, i);
	  tc = TREE_CODE (operand);

	  if (TREE_CODE_CLASS (tc) == 't')
	    /* We can get here with sizeof, e.g.:
		     
	       template <class T> void f(A<sizeof(T)>);  */
	    build_mangled_name_for_type (operand);
	  else
	    build_overload_value (TREE_TYPE (operand),
				  operand,
				  mf_maybe_uninstantiated);
	}
    }
  else
    {
      /* We don't ever want this output, but it's
	 inconvenient not to be able to build the string.
	 This should cause assembler errors we'll notice.  */
	    
      static int n;
    bad_value:
      sprintf (digit_buffer, " *%d", n++);
      OB_PUTCP (digit_buffer);
    }

  OB_PUTC ('W');
  numeric_output_need_bar = 0;
}

/* Encoding for an INTEGER_CST value.  */

static void
build_overload_int (value, flags)
     tree value;
     mangling_flags flags;
{
  int multiple_words_p = 0;
  int multiple_digits_p = 0;

  if ((flags & mf_maybe_uninstantiated) && TREE_CODE (value) != INTEGER_CST)
    {
      mangle_expression (value);
      return;
    }

  /* Unless we were looking at an uninstantiated template, integers
     should always be represented by constants.  */
  my_friendly_assert (TREE_CODE (value) == INTEGER_CST, 243);

  /* If value doesn't fit in a single HOST_WIDE_INT, we must use a
     special output routine that can deal with this.  */
  if (! host_integerp (value, 0))
    {
      multiple_words_p = 1;
      /* And there is certainly going to be more than one digit.  */
      multiple_digits_p = 1;
    }
  else 
    multiple_digits_p = ((HOST_WIDE_INT) TREE_INT_CST_LOW (value) > 9
			 || (HOST_WIDE_INT) TREE_INT_CST_LOW (value) < -9);

  /* If necessary, add a leading underscore.  */
  if (multiple_digits_p && (flags & mf_use_underscores_around_value))
    OB_PUTC ('_');

  /* Output the number itself.  */
  if (multiple_words_p)
    dicat (TREE_INT_CST_LOW (value), TREE_INT_CST_HIGH (value));
  else
    icat (TREE_INT_CST_LOW (value));

  if (flags & mf_use_underscores_around_value)
    {
      if (multiple_digits_p)
	OB_PUTC ('_');
      /* Whether or not there were multiple digits, we don't need an
	 underscore.  We've either terminated the number with an
	 underscore, or else it only had one digit.  */
      numeric_output_need_bar = 0;
    }
  else
    /* We just output a numeric value.  */
    numeric_output_need_bar = 1;
}


/* Output S followed by a representation of the TEMPLATE_PARM_INDEX
   supplied in INDEX.  */

static void 
build_mangled_template_parm_index (s, index)
     const char *s;
     tree index;
{
  OB_PUTCP (s);
  build_underscore_int (TEMPLATE_PARM_IDX (index));
  /* We use the LEVEL, not the ORIG_LEVEL, because the mangling is a
     representation of the function from the point of view of its
     type.  */
  build_underscore_int (TEMPLATE_PARM_LEVEL (index));
}


/* Mangling for C9X integer types (and Cygnus extensions for 128-bit
   and other types) is based on the letter "I" followed by the hex
   representations of the bitsize for the type in question. For
   encodings that result in larger than two digits, a leading and
   trailing underscore is added.

   Thus:
   int1_t   = 001 = I01
   int8_t   = 008 = I08 
   int16_t  = 010 = I10
   int24_t  = 018 = I18
   int32_t  = 020 = I20
   int64_t  = 040 = I40
   int80_t  = 050 = I50
   int128_t = 080 = I80
   int256_t = 100 = I_100_
   int512_t = 200 = I_200_

   Given an integer in decimal format, mangle according to this scheme. */

#if HOST_BITS_PER_WIDE_INT >= 64
static void
build_mangled_C9x_name (bits)
     int bits;
{
  char mangled[10] = "";

  if (bits > 255)
    sprintf (mangled, "I_%x_", bits);
  else
    sprintf (mangled, "I%.2x", bits);

  OB_PUTCP (mangled);
}
#endif

static void
build_overload_value (type, value, flags)
     tree type, value;
     mangling_flags flags;
{
  my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (type)) == 't', 0);

  while (TREE_CODE (value) == NON_LVALUE_EXPR
	 || TREE_CODE (value) == NOP_EXPR)
    value = TREE_OPERAND (value, 0);

  if (numeric_output_need_bar)
    {
      OB_PUTC ('_');
      numeric_output_need_bar = 0;
    }

  if (TREE_CODE (value) == TEMPLATE_PARM_INDEX)
    {
      build_mangled_template_parm_index ("Y", value);
      return;
    }

  if (TYPE_PTRMEM_P (type))
    {
      if (TREE_CODE (value) != PTRMEM_CST)
	/* We should have already rejected this pointer to member,
	   since it is not a constant.  */
	my_friendly_abort (0);

      /* Get the actual FIELD_DECL.  */
      value = PTRMEM_CST_MEMBER (value);
      my_friendly_assert (TREE_CODE (value) == FIELD_DECL, 0);

      /* Output the name of the field.  */
      build_overload_identifier (DECL_NAME (value));
      return;
    }
  else if (INTEGRAL_TYPE_P (type))
    {
      build_overload_int (value, flags);
      return;
    }

  /* The only case where we use the extra underscores here is when
     forming the mangling for an integral non-type template argument.
     If that didn't happen, stop now.  */
  flags &= ~mf_use_underscores_around_value;

  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
      {
	REAL_VALUE_TYPE val;
	char *bufp = digit_buffer;

	/* We must handle non-constants in templates.  */
	if (TREE_CODE (value) != REAL_CST)
	  {
	    mangle_expression (value);
	    break;
	  }

	val = TREE_REAL_CST (value);
	if (REAL_VALUE_ISNAN (val))
	  {
	    sprintf (bufp, "NaN");
	  }
	else
	  {
	    if (REAL_VALUE_NEGATIVE (val))
	      {
		val = REAL_VALUE_NEGATE (val);
		*bufp++ = 'm';
	      }
	    if (REAL_VALUE_ISINF (val))
	      {
		sprintf (bufp, "Infinity");
	      }
	    else
	      {
		REAL_VALUE_TO_DECIMAL (val, "%.20e", bufp);
		bufp = (char *) index (bufp, 'e');
		if (!bufp)
		  strcat (digit_buffer, "e0");
		else
		  {
		    char *p;
		    bufp++;
		    if (*bufp == '-')
		      {
			*bufp++ = 'm';
		      }
		    p = bufp;
		    if (*p == '+')
		      p++;
		    while (*p == '0')
		      p++;
		    if (*p == 0)
		      {
			*bufp++ = '0';
			*bufp = 0;
		      }
		    else if (p != bufp)
		      {
			while (*p)
			  *bufp++ = *p++;
			*bufp = 0;
		      }
		  }
#ifdef NO_DOT_IN_LABEL
		bufp = (char *) index (bufp, '.');
		if (bufp)
		  *bufp = '_';
#endif
	      }
	  }
	OB_PUTCP (digit_buffer);
	numeric_output_need_bar = 1;
	return;
      }
    case POINTER_TYPE:
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  build_overload_int (value, flags);
	  return;
	}
      else if (TREE_CODE (value) == TEMPLATE_PARM_INDEX)
	{
	  build_mangled_template_parm_index ("", value);
	  numeric_output_need_bar = 1;
	  return;
	}

      value = TREE_OPERAND (value, 0);

      /* Fall through.  */

    case REFERENCE_TYPE:
      if (TREE_CODE (value) == VAR_DECL)
	{
	  my_friendly_assert (DECL_NAME (value) != 0, 245);
	  build_overload_identifier (DECL_ASSEMBLER_NAME (value));
	  return;
	}
      else if (TREE_CODE (value) == FUNCTION_DECL)
	{
	  my_friendly_assert (DECL_NAME (value) != 0, 246);
	  build_overload_identifier (DECL_ASSEMBLER_NAME (value));
	  return;
	}
      else if (TREE_CODE (value) == SCOPE_REF)
	build_overload_scope_ref (value);
      else
	my_friendly_abort (71);
      break; /* not really needed */

    case RECORD_TYPE:
      {
	tree delta;
	tree idx;
	tree pfn;
	tree delta2;
	tree fn;

	my_friendly_assert (TYPE_PTRMEMFUNC_P (type), 0);

	/* We'll get a ADDR_EXPR of a SCOPE_REF here if we're
	   mangling, an instantiation of something like:

	     template <class T, void (T::*fp)()> class C {};
	     template <class T> C<T, &T::f> x();  
	
	   We mangle the return type of the function, and that
	   contains template parameters.  */
	if (TREE_CODE (value) == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (value, 0)) == SCOPE_REF)
	  {
	    build_overload_scope_ref (TREE_OPERAND (value, 0));
	    break;
	  }

	my_friendly_assert (TREE_CODE (value) == PTRMEM_CST, 0);

	expand_ptrmemfunc_cst (value, &delta, &idx, &pfn, &delta2);
	fn = PTRMEM_CST_MEMBER (value);
	build_overload_int (delta, flags);
	OB_PUTC ('_');
	if (!flag_new_abi)
	  {
	    build_overload_int (idx, flags);
	    OB_PUTC ('_');
	  }
	else if (DECL_VIRTUAL_P (fn))
	  {
	    build_overload_int (DECL_VINDEX (fn), flags);
	    OB_PUTC ('_');
	  }

	if (!DECL_VIRTUAL_P (fn))
	  {
	    numeric_output_need_bar = 0;
	    build_overload_identifier (DECL_ASSEMBLER_NAME (fn));
	  }
	else if (!flag_new_abi)
	  {
	    OB_PUTC ('i');
	    build_overload_int (delta2, flags);
	  }
      }
      break;
      
    default:
      sorry ("conversion of %s as template parameter",
	     tree_code_name [(int) TREE_CODE (type)]);
      my_friendly_abort (72);
    }
}


/* Add encodings for the declaration of template template parameters.
   PARMLIST must be a TREE_VEC.  */

static void
build_template_template_parm_names (parmlist)
     tree parmlist;
{
  int i, nparms;

  my_friendly_assert (TREE_CODE (parmlist) == TREE_VEC, 246.5);
  nparms = TREE_VEC_LENGTH (parmlist);
  icat (nparms);
  for (i = 0; i < nparms; i++)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parmlist, i));
      if (TREE_CODE (parm) == TYPE_DECL)
	{
	  /* This parameter is a type.  */
	  OB_PUTC ('Z');
	}
      else if (TREE_CODE (parm) == TEMPLATE_DECL)
	{
	  /* This parameter is a template. */
	  OB_PUTC ('z');
	  build_template_template_parm_names (DECL_INNERMOST_TEMPLATE_PARMS (parm));
	}
      else
	/* It's a PARM_DECL.  */
	build_mangled_name_for_type (TREE_TYPE (parm));
    }
}


/* Add encodings for the vector of template parameters in PARMLIST,
   given the vector of arguments to be substituted in ARGLIST.  */

static void
build_template_parm_names (parmlist, arglist)
     tree parmlist;
     tree arglist;
{
  int i, nparms;
  tree inner_args = innermost_args (arglist);

  nparms = TREE_VEC_LENGTH (parmlist);
  icat (nparms);
  for (i = 0; i < nparms; i++)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parmlist, i));
      tree arg = TREE_VEC_ELT (inner_args, i);
      if (TREE_CODE (parm) == TYPE_DECL)
	{
	  /* This parameter is a type.  */
	  OB_PUTC ('Z');
	  build_mangled_name_for_type (arg);
	}
      else if (TREE_CODE (parm) == TEMPLATE_DECL)
	{
	  /* This parameter is a template.  */
	  if (TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM)
	    /* Output parameter declaration, argument index and level.  */
	    build_mangled_name_for_type (arg);
	  else
	    {
	      /* A TEMPLATE_DECL node, output the parameter declaration 
		 and template name */

	      OB_PUTC ('z');
	      build_template_template_parm_names
		(DECL_INNERMOST_TEMPLATE_PARMS (parm));
	      icat (IDENTIFIER_LENGTH (DECL_NAME (arg)));
	      OB_PUTID (DECL_NAME (arg));
	    }
	}
      else
	{
	  parm = tsubst (parm, arglist, /*complain=*/1, NULL_TREE);
	  /* It's a PARM_DECL.  */
	  build_mangled_name_for_type (TREE_TYPE (parm));
	  build_overload_value (TREE_TYPE (parm), arg, 
				((mf_maybe_uninstantiated 
				  * uses_template_parms (arglist))
				 | mf_use_underscores_around_value));
	}
    }
 }

/* Output the representation for NAME, which is either a TYPE_DECL or
   an IDENTIFIER.  */

static void
build_overload_identifier (name)
     tree name;
{
  if (TREE_CODE (name) == TYPE_DECL
      && CLASS_TYPE_P (TREE_TYPE (name))
      && CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (name))
      && (PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (TREE_TYPE (name)))
	  || (TREE_CODE (CP_DECL_CONTEXT (CLASSTYPE_TI_TEMPLATE 
					  (TREE_TYPE (name))))
	      == FUNCTION_DECL)))
    {
      /* NAME is the TYPE_DECL for a template specialization.  */
      tree template, parmlist, arglist, tname;
      template = CLASSTYPE_TI_TEMPLATE (TREE_TYPE (name));
      arglist = CLASSTYPE_TI_ARGS (TREE_TYPE (name));
      tname = DECL_NAME (template);
      parmlist = DECL_INNERMOST_TEMPLATE_PARMS (template);
      OB_PUTC ('t');
      icat (IDENTIFIER_LENGTH (tname));
      OB_PUTID (tname);
      build_template_parm_names (parmlist, arglist);
    }
  else
    {
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
      if (numeric_output_need_bar)
	{
	  OB_PUTC ('_');
	  numeric_output_need_bar = 0;
	}
      icat (IDENTIFIER_LENGTH (name));
      OB_PUTID (name);
    }
}

/* Given DECL, either a class TYPE, TYPE_DECL or FUNCTION_DECL, produce
   the mangling for it.  Used by build_mangled_name and build_static_name.  */

static void
build_qualified_name (decl)
     tree decl;
{
  tree context;
  int i = 1;

  if (TREE_CODE_CLASS (TREE_CODE (decl)) == 't')
    decl = TYPE_NAME (decl);

  /* If DECL_ASSEMBLER_NAME has been set properly, use it.  */
  if (TREE_CODE (decl) == TYPE_DECL
      && DECL_ASSEMBLER_NAME (decl) != DECL_NAME (decl) && !flag_do_squangling)
    {
      tree id = DECL_ASSEMBLER_NAME (decl);
      OB_PUTID (id);
      if (ISDIGIT (IDENTIFIER_POINTER (id) [IDENTIFIER_LENGTH (id) - 1]))
	numeric_output_need_bar = 1;
      return;
    }

  context = decl;
  /* If we can't find a Ktype, do it the hard way.  */
  if (check_ktype (context, FALSE) == -1)
    {
      /* Count type and namespace scopes.  */
      while (1)
	{
	  context = CP_DECL_CONTEXT (context);
	  if (context == global_namespace)
	    break;
	  i += 1;
	  if (check_ktype (context, FALSE) != -1)
	    /* Found one!  */
	    break;
	  if (TREE_CODE_CLASS (TREE_CODE (context)) == 't')
	    context = TYPE_NAME (context);
	}
    }

  if (i > 1)
    {
      OB_PUTC ('Q');
      build_underscore_int (i);
      numeric_output_need_bar = 0;
    }
  build_overload_nested_name (decl);
}

/* Output the mangled representation for TYPE.  If EXTRA_GCODE is
   non-zero, mangled names for structure/union types are intentionally
   mangled differently from the method described in the ARM.  */

static void 
build_mangled_name_for_type_with_Gcode (type, extra_Gcode)
     tree type;
     int extra_Gcode;
{
  if (TYPE_PTRMEMFUNC_P (type))
    type = TYPE_PTRMEMFUNC_FN_TYPE (type);
  process_modifiers (type);
  process_overload_item (type, extra_Gcode);
}

/* Like build_mangled_name_for_type_with_Gcode, but never outputs the
   `G'.  */

static void
build_mangled_name_for_type (type)
     tree type;
{
  build_mangled_name_for_type_with_Gcode (type, 0);
}

/* Given a list of parameters in PARMTYPES, create an unambiguous
   overload string. Should distinguish any type that C (or C++) can
   distinguish. I.e., pointers to functions are treated correctly.

   Caller must deal with whether a final `e' goes on the end or not.

   Any default conversions must take place before this function
   is called.

   BEGIN and END control initialization and finalization of the
   obstack where we build the string.  */

char *
build_overload_name (parmtypes, begin, end)
     tree parmtypes;
     int begin, end;
{
  char *ret;
  start_squangling ();
  ret = build_mangled_name (parmtypes, begin, end);
  end_squangling ();
  return ret ;
}

/* Output the mangled representation for PARMTYPES.  If PARMTYPES is a
   TREE_LIST, then it is a list of parameter types.  Otherwise,
   PARMTYPES must be a single type.  */

static char *
build_mangled_name (parmtypes, begin, end)
     tree parmtypes;
     int begin, end;
{
  if (begin) 
    OB_INIT ();

  if (TREE_CODE (parmtypes) != TREE_LIST)  
    /* There is only one type.  */
    build_mangled_name_for_type (parmtypes);
  else  
    {
      /* There are several types in a parameter list.  */
      int nrepeats = 0;
      int old_style_repeats = !flag_do_squangling && !nofold && typevec;
      tree last_type = NULL_TREE;

      for (; parmtypes && parmtypes != void_list_node;
	   parmtypes = TREE_CHAIN (parmtypes))
	{
	  /* We used to call canonical_type_variant here, but that isn't
	     good enough; it doesn't handle pointers to typedef types.  So
	     we can't just set TREE_USED to say we've seen a type already;
	     we have to check each of the earlier types with same_type_p.  */
	  tree parmtype = TREE_VALUE (parmtypes);

	  if (old_style_repeats)
	    {
	      /* Every argument gets counted.  */
	      my_friendly_assert (maxtype < VARRAY_SIZE (typevec), 387);
	      VARRAY_TREE (typevec, maxtype) = parmtype;
	      maxtype++;
	    }

	  if (last_type && same_type_p (parmtype, last_type))
	    {
	      if (flag_do_squangling 
		  || (old_style_repeats
		      && is_back_referenceable_type (parmtype)))
		{
		  /* The next type is the same as this one.  Keep
		     track of the repetition, and output the repeat
		     count later.  */
		  nrepeats++;
		  continue;
		}
	    }
	  else if (nrepeats != 0)
	    {
	      /* Indicate how many times the previous parameter was
		 repeated.  */
	      if (old_style_repeats)
		flush_repeats (nrepeats, last_type);
	      else
		issue_nrepeats (nrepeats, last_type);
	      nrepeats = 0;
	    }
	  
	  last_type = parmtype;

	  /* Note that for bug-compatibility with 2.7.2, we can't build up
	     repeats of types other than the most recent one.  So we call
	     flush_repeats every round, if we get this far.  */
	  if (old_style_repeats && flush_repeats (0, parmtype))
	    continue;

	  /* Output the PARMTYPE.  */
	  build_mangled_name_for_type_with_Gcode (parmtype, 1);
	}

      /* Output the repeat count for the last parameter, if
	 necessary.  */
      if (nrepeats != 0)
	{
	  if (old_style_repeats)
	    flush_repeats (nrepeats, last_type);
	  else
	    issue_nrepeats (nrepeats, last_type);
	  nrepeats = 0;
	}

      if (!parmtypes)
	/* The parameter list ends in an ellipsis.  */
	OB_PUTC ('e');
    }

  if (end) 
    OB_FINISH ();
  return (char *)obstack_base (&scratch_obstack);
}

/* Emit modifiers such as constant, read-only, and volatile.  */

static void 
process_modifiers (parmtype) 
     tree parmtype;
{
  /* Note that here we do not use CP_TYPE_CONST_P and friends because
     we describe types recursively; we will get the `const' in 
     `const int ()[10]' when processing the `const int' part.  */
  if (TYPE_READONLY (parmtype))
    OB_PUTC ('C');
  if (TREE_CODE (parmtype) == INTEGER_TYPE
      && parmtype != char_type_node
      && parmtype != wchar_type_node
      && (TYPE_MAIN_VARIANT (parmtype)
	  == unsigned_type (TYPE_MAIN_VARIANT (parmtype)))
      && ! TYPE_FOR_JAVA (parmtype))
    OB_PUTC ('U');
  if (TYPE_VOLATILE (parmtype))
    OB_PUTC ('V');
  /* It would be better to use `R' for `restrict', but that's already
     used for reference types.  And `r' is used for `long double'.  */
  if (TYPE_RESTRICT (parmtype))
    OB_PUTC ('u');
}

/* Check to see if TYPE has been entered into the Bcode typelist.  If
   so, return 1 and emit a backreference to TYPE.  Otherwise, add TYPE
   to the list of back-referenceable types and return 0.  */

static int 
check_btype (type) 
     tree type;
{
  size_t x;

  if (btypelist == NULL)
    return 0;

  if (!is_back_referenceable_type (type))
    return 0;

  for (x = 0; x < maxbtype; x++) 
    if (same_type_p (type, VARRAY_TREE (btypelist, x)))
      {
	OB_PUTC ('B');
	icat (x);
	if (x > 9)
	  OB_PUTC ('_');
	return 1 ;
      }

  if (VARRAY_SIZE (btypelist) <= maxbtype) 
    /* Enlarge the table.  */
    VARRAY_GROW (btypelist,
		 VARRAY_SIZE (btypelist) * 3 / 2);

  /* Register the TYPE.  */
  VARRAY_TREE (btypelist, maxbtype) = type;
  maxbtype++;

  return 0;
}

/* Emit the correct code for various node types.  */

static void 
process_overload_item (parmtype, extra_Gcode) 
  tree parmtype;
  int extra_Gcode;
{
  numeric_output_need_bar = 0;

  /* Our caller should have already handed any qualifiers, so pull out the
     TYPE_MAIN_VARIANT to avoid typedef confusion.  Except we can't do that
     for arrays, because they are transparent to qualifiers.  Sigh.  */
  if (TREE_CODE (parmtype) == ARRAY_TYPE)
    parmtype = canonical_type_variant (parmtype);
  else
    parmtype = TYPE_MAIN_VARIANT (parmtype);

  /* These tree types are considered modifiers for B code squangling,
     and therefore should not get entries in the Btypelist.  They are,
     however, repeatable types.  */

  switch (TREE_CODE (parmtype))
    {
    case REFERENCE_TYPE:
      OB_PUTC ('R');
      goto more;

    case ARRAY_TYPE:
      {
        OB_PUTC ('A');
        if (TYPE_DOMAIN (parmtype) == NULL_TREE)
	  OB_PUTC ('_');
        else
	  {
	    tree length = array_type_nelts (parmtype);
	    if (TREE_CODE (length) != INTEGER_CST || flag_do_squangling)
	      {
		length = fold (build (PLUS_EXPR, TREE_TYPE (length),
				      length, integer_one_node));
		STRIP_NOPS (length);
	      }
	    build_overload_value (sizetype, length, 1);
	  }
	if (numeric_output_need_bar && ! flag_do_squangling)
	  OB_PUTC ('_');
        goto more;
      }

    case POINTER_TYPE:
      OB_PUTC ('P');
    more:
      build_mangled_name_for_type (TREE_TYPE (parmtype));
      return;
      break;

    default:
      break;
    }
  
  if (flag_do_squangling && check_btype (parmtype)) 
    /* If PARMTYPE is already in the list of back-referenceable types,
       then check_btype will output the appropriate reference, and
       there's nothing more to do.  */
    return;

  switch (TREE_CODE (parmtype))
    {
    case OFFSET_TYPE:
      OB_PUTC ('O');
      build_mangled_name_for_type (TYPE_OFFSET_BASETYPE (parmtype));
      OB_PUTC ('_');
      build_mangled_name_for_type (TREE_TYPE (parmtype));
      break;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
        tree parms = TYPE_ARG_TYPES (parmtype);

	/* Rather than implementing a reentrant TYPEVEC, we turn off
	   repeat codes here, unless we're squangling.  Squangling
	   doesn't make use of the TYPEVEC, so there's no reentrancy
	   problem.  */
	int old_nofold = nofold;
	if (!flag_do_squangling)
	  nofold = 1;

	if (TREE_CODE (parmtype) == METHOD_TYPE)
	  {
	    /* Mark this as a method.  */
            OB_PUTC ('M');
	    /* Output the class of which this method is a member.  */
            build_mangled_name_for_type (TYPE_METHOD_BASETYPE (parmtype));
	    /* Output any qualifiers for the `this' parameter.  */
	    process_modifiers (TREE_TYPE (TREE_VALUE (parms)));
	  }

	/* Output the parameter types.  */
	OB_PUTC ('F');
	if (parms == NULL_TREE)
	  OB_PUTC ('e');
	else if (parms == void_list_node)
	  OB_PUTC ('v');
	else
	  build_mangled_name (parms, 0, 0);

        /* Output the return type.  */
        OB_PUTC ('_');
        build_mangled_name_for_type (TREE_TYPE (parmtype));

        nofold = old_nofold;
        break;
      }

    case INTEGER_TYPE:
      if (parmtype == integer_type_node
          || parmtype == unsigned_type_node
	  || parmtype == java_int_type_node)
        OB_PUTC ('i');
      else if (parmtype == long_integer_type_node
               || parmtype == long_unsigned_type_node)
        OB_PUTC ('l');
      else if (parmtype == short_integer_type_node
               || parmtype == short_unsigned_type_node
	       || parmtype == java_short_type_node)
        OB_PUTC ('s');
      else if (parmtype == signed_char_type_node)
        {
          OB_PUTC ('S');
          OB_PUTC ('c');
        }
      else if (parmtype == char_type_node
               || parmtype == unsigned_char_type_node
	       || parmtype == java_byte_type_node)
        OB_PUTC ('c');
      else if (parmtype == wchar_type_node
	       || parmtype == java_char_type_node)
        OB_PUTC ('w');
      else if (parmtype == long_long_integer_type_node
	       || parmtype == long_long_unsigned_type_node
	       || parmtype == java_long_type_node)
        OB_PUTC ('x');
      else if (parmtype == java_boolean_type_node)
	OB_PUTC ('b');
#if HOST_BITS_PER_WIDE_INT >= 64
      else
	{
	  int bits = TREE_INT_CST_LOW (TYPE_SIZE (parmtype));
	  build_mangled_C9x_name (bits);
	}
#else
      else
	my_friendly_abort (73);
#endif
      break;

    case BOOLEAN_TYPE:
      OB_PUTC ('b');
      break;

    case REAL_TYPE:
      if (parmtype == long_double_type_node)
        OB_PUTC ('r');
      else if (parmtype == double_type_node
	       || parmtype == java_double_type_node)
        OB_PUTC ('d');
      else if (parmtype == float_type_node
	       || parmtype == java_float_type_node)
        OB_PUTC ('f');
      else my_friendly_abort (74);
      break;

    case COMPLEX_TYPE:
      OB_PUTC ('J');
      build_mangled_name_for_type (TREE_TYPE (parmtype));
      break;

    case VOID_TYPE:
      OB_PUTC ('v');
      break;

    case ERROR_MARK:	/* not right, but nothing is anyway */
      break;

      /* have to do these */
    case UNION_TYPE:
    case RECORD_TYPE:
      {   
        if (extra_Gcode)
          OB_PUTC ('G');       /* make it look incompatible with AT&T */
        /* drop through into next case */
      }
    case ENUMERAL_TYPE:
      {
        tree name = TYPE_NAME (parmtype);

        my_friendly_assert (TREE_CODE (name) == TYPE_DECL, 248);

        build_qualified_name (name);
        break;
      }

    case UNKNOWN_TYPE:
      /* This will take some work.  */
      OB_PUTC ('?');
      break;

    case TEMPLATE_TEMPLATE_PARM:
      /* Find and output the original template parameter 
         declaration. */
      if (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (parmtype))
        {
	  build_mangled_template_parm_index ("tzX",
					     TEMPLATE_TYPE_PARM_INDEX 
					     (parmtype));
          build_template_parm_names
            (DECL_INNERMOST_TEMPLATE_PARMS (TYPE_TI_TEMPLATE (parmtype)),
	     TYPE_TI_ARGS (parmtype));
        }
      else
        {
	  build_mangled_template_parm_index ("ZzX",
					     TEMPLATE_TYPE_PARM_INDEX 
					     (parmtype));
          build_template_template_parm_names
            (DECL_INNERMOST_TEMPLATE_PARMS (TYPE_STUB_DECL (parmtype)));
        }
      break;

    case TEMPLATE_TYPE_PARM:
      build_mangled_template_parm_index ("X", 
					 TEMPLATE_TYPE_PARM_INDEX
					 (parmtype));
      break;
        
    case TYPENAME_TYPE:
      /* When mangling the type of a function template whose
         declaration looks like:

         template <class T> void foo(typename T::U)
         
         we have to mangle these.  */
      build_qualified_name (parmtype);
      break;

    default:
      my_friendly_abort (75);
    }

}

/* Produce the mangling for a variable named NAME in CONTEXT, which can
   be either a class TYPE or a FUNCTION_DECL.  */

tree
build_static_name (context, name)
     tree context, name;
{
  OB_INIT ();
  numeric_output_need_bar = 0;
  start_squangling ();
#ifdef JOINER
  OB_PUTC ('_');
  build_qualified_name (context);
  OB_PUTC (JOINER);
#else
  OB_PUTS ("__static_");
  build_qualified_name (context);
  OB_PUTC ('_');
#endif
  OB_PUTID (name);
  OB_FINISH ();
  end_squangling ();

  return get_identifier ((char *)obstack_base (&scratch_obstack));
}

/* FOR_METHOD should be 1 if the declaration in question is for a member
   of a class (including a static member) and 2 if the declaration is
   for a constructor.  */
tree 
build_decl_overload_real (dname, parms, ret_type, tparms, targs,
			  for_method) 
     tree dname;
     tree parms;
     tree ret_type;
     tree tparms;
     tree targs;
     int for_method;
{
  const char *name = IDENTIFIER_POINTER (dname);

  /* member operators new and delete look like methods at this point.  */
  if (! for_method && current_namespace == global_namespace
      && parms != NULL_TREE && TREE_CODE (parms) == TREE_LIST
      && TREE_CHAIN (parms) == void_list_node)
    {
      if (dname == ansi_opname[(int) DELETE_EXPR])
	return get_identifier ("__builtin_delete");
      else if (dname == ansi_opname[(int) VEC_DELETE_EXPR])
	return get_identifier ("__builtin_vec_delete");
      if (dname == ansi_opname[(int) NEW_EXPR])
	return get_identifier ("__builtin_new");
      else if (dname == ansi_opname[(int) VEC_NEW_EXPR])
	return get_identifier ("__builtin_vec_new");
    }

  start_squangling ();
  OB_INIT ();
  if (for_method != 2)
    OB_PUTCP (name);
  /* Otherwise, we can divine that this is a constructor,
     and figure out its name without any extra encoding.  */

  OB_PUTC2 ('_', '_');
  numeric_output_need_bar = 0;

  if (tparms)
    {
      OB_PUTC ('H');
      build_template_parm_names (tparms, targs);
      OB_PUTC ('_');
    }
  else if (!for_method && current_namespace == global_namespace)
    /* XXX this works only if we call this in the same namespace
       as the declaration. Unfortunately, we don't have the _DECL,
       only its name */
    OB_PUTC ('F');

  if (!for_method && current_namespace != global_namespace)
    /* qualify with namespace */
    build_qualified_name (current_namespace);

  if (parms == NULL_TREE)
    OB_PUTC ('e');
  else if (parms == void_list_node)
    OB_PUTC ('v');
  else
    {
      if (!flag_do_squangling)
        {
	  /* Allocate typevec array.  */
	  size_t typevec_size = list_length (parms);
          maxtype = 0;
	  if (!for_method && current_namespace != global_namespace)
	    /* The namespace of a global function needs one slot.  */
	    typevec_size++;
	  VARRAY_TREE_INIT (typevec, typevec_size, "typevec");
        }
      nofold = 0;

      if (for_method)
	{
	  tree this_type = TREE_TYPE (TREE_VALUE (parms));

	  build_mangled_name_for_type (this_type);

          if (!flag_do_squangling) 
	    {
	      my_friendly_assert (maxtype < VARRAY_SIZE (typevec), 387);
	      VARRAY_TREE (typevec, maxtype) = this_type;
	      maxtype++;
	    }

	  if (TREE_CHAIN (parms))
	    build_mangled_name (TREE_CHAIN (parms), 0, 0);
	  else
	    OB_PUTC ('e');
	}
      else
	{
	  /* the namespace qualifier for a global function 
	     will count as type */
	  if (current_namespace != global_namespace
	      && !flag_do_squangling)
	    {
	      my_friendly_assert (maxtype < VARRAY_SIZE (typevec), 387);
	      VARRAY_TREE (typevec, maxtype) = current_namespace;
	      maxtype++;
	    }
	  build_mangled_name (parms, 0, 0);
	}

      if (!flag_do_squangling)
	/* Deallocate typevec array.  */
	VARRAY_FREE (typevec);
    }

  if (ret_type != NULL_TREE && for_method != 2)
    {
      /* Add the return type. */
      OB_PUTC ('_');
      build_mangled_name_for_type (ret_type);
    }

  OB_FINISH ();
  end_squangling ();
  {
    tree n = get_identifier (obstack_base (&scratch_obstack));
    if (IDENTIFIER_OPNAME_P (dname))
      IDENTIFIER_OPNAME_P (n) = 1;
    return n;
  }
}

/* Change the name of a function definition so that it may be
   overloaded. NAME is the name of the function to overload,
   PARMS is the parameter list (which determines what name the
   final function obtains).

   FOR_METHOD is 1 if this overload is being performed
   for a method, rather than a function type.  It is 2 if
   this overload is being performed for a constructor.  */

tree
build_decl_overload (dname, parms, for_method)
     tree dname;
     tree parms;
     int for_method;
{
  return build_decl_overload_real (dname, parms, NULL_TREE, NULL_TREE,
				   NULL_TREE, for_method); 
}

/* Set the mangled name (DECL_ASSEMBLER_NAME) for DECL.  */

void
set_mangled_name_for_decl (decl)
     tree decl;
{
  tree parm_types;

  if (processing_template_decl)
    /* There's no need to mangle the name of a template function.  */
    return;

  parm_types = TYPE_ARG_TYPES (TREE_TYPE (decl));

  if (DECL_STATIC_FUNCTION_P (decl))
    parm_types = 
      hash_tree_chain (build_pointer_type (DECL_CONTEXT (decl)),
					   parm_types);
  else
    /* The only member functions whose type is a FUNCTION_TYPE, rather
       than a METHOD_TYPE, should be static members.  */
    my_friendly_assert (!DECL_CONTEXT (decl)
			|| !IS_AGGR_TYPE_CODE (TREE_CODE (DECL_CONTEXT (decl)))
			|| TREE_CODE (TREE_TYPE (decl)) != FUNCTION_TYPE,
			0);

  DECL_ASSEMBLER_NAME (decl)
    = build_decl_overload (DECL_NAME (decl), parm_types, 
			   DECL_FUNCTION_MEMBER_P (decl)
			   + DECL_CONSTRUCTOR_P (decl));
}

/* Build an overload name for the type expression TYPE.  */

tree
build_typename_overload (type)
     tree type;
{
  tree id;

  OB_INIT ();
  OB_PUTID (ansi_opname[(int) TYPE_EXPR]);
  nofold = 1;
  start_squangling ();
  build_mangled_name (type, 0, 1);
  id = get_identifier (obstack_base (&scratch_obstack));
  IDENTIFIER_OPNAME_P (id) = 1;
#if 0
  IDENTIFIER_GLOBAL_VALUE (id) = TYPE_MAIN_DECL (type);
#endif
  TREE_TYPE (id) = type;
  end_squangling ();
  return id;
}

tree
build_overload_with_type (name, type)
     tree name, type;
{
  OB_INIT ();
  OB_PUTID (name);
  nofold = 1;

  start_squangling ();
  build_mangled_name (type, 0, 1);
  end_squangling ();
  return get_identifier (obstack_base (&scratch_obstack));
}

tree
get_id_2 (name, name2)
     const char *name;
     tree name2;
{
  OB_INIT ();
  OB_PUTCP (name);
  OB_PUTID (name2);
  OB_FINISH ();
  return get_identifier (obstack_base (&scratch_obstack));
}

/* Returns a DECL_ASSEMBLER_NAME for the destructor of type TYPE.  */

tree
build_destructor_name (type)
     tree type;
{
  return build_overload_with_type (get_identifier (DESTRUCTOR_DECL_PREFIX),
				   type);
}

/* Given a tree_code CODE, and some arguments (at least one),
   attempt to use an overloaded operator on the arguments.

   For unary operators, only the first argument need be checked.
   For binary operators, both arguments may need to be checked.

   Member functions can convert class references to class pointers,
   for one-level deep indirection.  More than that is not supported.
   Operators [](), ()(), and ->() must be member functions.

   We call function call building calls with LOOKUP_COMPLAIN if they
   are our only hope.  This is true when we see a vanilla operator
   applied to something of aggregate type.  If this fails, we are free
   to return `error_mark_node', because we will have reported the
   error.

   Operators NEW and DELETE overload in funny ways: operator new takes
   a single `size' parameter, and operator delete takes a pointer to the
   storage being deleted.  When overloading these operators, success is
   assumed.  If there is a failure, report an error message and return
   `error_mark_node'.  */

/* NOSTRICT */
tree
build_opfncall (code, flags, xarg1, xarg2, arg3)
     enum tree_code code;
     int flags;
     tree xarg1, xarg2, arg3;
{
  return build_new_op (code, flags, xarg1, xarg2, arg3);
}

/* This function takes an identifier, ID, and attempts to figure out what
   it means. There are a number of possible scenarios, presented in increasing
   order of hair:

   1) not in a class's scope
   2) in class's scope, member name of the class's method
   3) in class's scope, but not a member name of the class
   4) in class's scope, member name of a class's variable

   NAME is $1 from the bison rule. It is an IDENTIFIER_NODE.
   VALUE is $$ from the bison rule. It is the value returned by lookup_name ($1)

   As a last ditch, try to look up the name as a label and return that
   address.

   Values which are declared as being of REFERENCE_TYPE are
   automatically dereferenced here (as a hack to make the
   compiler faster).  */

tree
hack_identifier (value, name)
     tree value, name;
{
  tree type;

  if (value == error_mark_node)
    {
      if (current_class_name)
	{
	  tree fields = lookup_fnfields (TYPE_BINFO (current_class_type), name, 1);
	  if (fields == error_mark_node)
	    return error_mark_node;
	  if (fields)
	    {
	      tree fndecl;

	      fndecl = TREE_VALUE (fields);
	      my_friendly_assert (TREE_CODE (fndecl) == FUNCTION_DECL, 251);
	      /* I could not trigger this code. MvL */
	      my_friendly_abort (980325);
#ifdef DEAD
	      if (DECL_CHAIN (fndecl) == NULL_TREE)
		{
		  warning ("methods cannot be converted to function pointers");
		  return fndecl;
		}
	      else
		{
		  error ("ambiguous request for method pointer `%s'",
			 IDENTIFIER_POINTER (name));
		  return error_mark_node;
		}
#endif
	    }
	}
      if (flag_labels_ok && IDENTIFIER_LABEL_VALUE (name))
	{
	  return IDENTIFIER_LABEL_VALUE (name);
	}
      return error_mark_node;
    }

  type = TREE_TYPE (value);
  if (TREE_CODE (value) == FIELD_DECL)
    {
      if (current_class_ptr == NULL_TREE)
	{
	  if (current_function_decl 
	      && DECL_STATIC_FUNCTION_P (current_function_decl))
	    cp_error ("invalid use of member `%D' in static member function",
		      value);
	  else
	    /* We can get here when processing a bad default
	       argument, like:
	         struct S { int a; void f(int i = a); }  */
	    cp_error ("invalid use of member `%D'", value);

	  return error_mark_node;
	}
      TREE_USED (current_class_ptr) = 1;

      /* Mark so that if we are in a constructor, and then find that
	 this field was initialized by a base initializer,
	 we can emit an error message.  */
      TREE_USED (value) = 1;
      value = build_component_ref (current_class_ref, name, NULL_TREE, 1);
    }
  else if ((TREE_CODE (value) == FUNCTION_DECL
	    && DECL_FUNCTION_MEMBER_P (value))
	   || (TREE_CODE (value) == OVERLOAD
	       && DECL_FUNCTION_MEMBER_P (OVL_CURRENT (value))))
    {
      tree decl;

      if (TREE_CODE (value) == OVERLOAD)
	value = OVL_CURRENT (value);

      decl = maybe_dummy_object (DECL_CONTEXT (value), 0);
      value = build_component_ref (decl, name, NULL_TREE, 1);
    }
  else if (really_overloaded_fn (value))
    ;
  else if (TREE_CODE (value) == OVERLOAD)
    /* not really overloaded function */
    mark_used (OVL_FUNCTION (value));
  else if (TREE_CODE (value) == TREE_LIST)
    {
      /* Ambiguous reference to base members, possibly other cases?.  */
      tree t = value;
      while (t && TREE_CODE (t) == TREE_LIST)
	{
	  mark_used (TREE_VALUE (t));
	  t = TREE_CHAIN (t);
	}
    }
  else if (TREE_CODE (value) == NAMESPACE_DECL)
    {
      cp_error ("use of namespace `%D' as expression", value);
      return error_mark_node;
    }
  else if (DECL_CLASS_TEMPLATE_P (value))
    {
      cp_error ("use of class template `%T' as expression", value);
      return error_mark_node;
    }
  else
    mark_used (value);

  if (TREE_CODE (value) == VAR_DECL || TREE_CODE (value) == PARM_DECL
      || TREE_CODE (value) == RESULT_DECL)
    {
      tree context = decl_function_context (value);
      if (context != NULL_TREE && context != current_function_decl
	  && ! TREE_STATIC (value))
	{
	  cp_error ("use of %s from containing function",
		      (TREE_CODE (value) == VAR_DECL
		       ? "`auto' variable" : "parameter"));
	  cp_error_at ("  `%#D' declared here", value);
	  value = error_mark_node;
	}
    }

  if (TREE_CODE_CLASS (TREE_CODE (value)) == 'd' && DECL_NONLOCAL (value))
    {
      if (DECL_CLASS_SCOPE_P (value)
	  && DECL_CONTEXT (value) != current_class_type)
	{
	  tree path;
	  path = currently_open_derived_class (DECL_CONTEXT (value));
	  enforce_access (path, value);
	}
    }
  else if (TREE_CODE (value) == TREE_LIST 
	   && TREE_TYPE (value) == error_mark_node)
    {
      error ("request for member `%s' is ambiguous in multiple inheritance lattice",
	     IDENTIFIER_POINTER (name));
      print_candidates (value);
      return error_mark_node;
    }

  if (! processing_template_decl)
    value = convert_from_reference (value);
  return value;
}


tree
make_thunk (function, delta, vcall_index)
     tree function;
     int delta;
     int vcall_index;
{
  tree thunk_id;
  tree thunk;
  tree func_decl;

  if (TREE_CODE (function) != ADDR_EXPR)
    abort ();
  func_decl = TREE_OPERAND (function, 0);
  if (TREE_CODE (func_decl) != FUNCTION_DECL)
    abort ();

  OB_INIT ();
  OB_PUTS ("__thunk_");
  if (delta > 0)
    {
      OB_PUTC ('n');
      icat (delta);
    }
  else
    icat (-delta);
  OB_PUTC ('_');
  OB_PUTID (DECL_ASSEMBLER_NAME (func_decl));
  if (vcall_index)
    {
      OB_PUTC ('_');
      icat (vcall_index);
    }
  OB_FINISH ();
  thunk_id = get_identifier (obstack_base (&scratch_obstack));

  thunk = IDENTIFIER_GLOBAL_VALUE (thunk_id);
  if (thunk && TREE_CODE (thunk) != THUNK_DECL)
    {
      cp_error ("implementation-reserved name `%D' used", thunk_id);
      thunk = NULL_TREE;
      SET_IDENTIFIER_GLOBAL_VALUE (thunk_id, thunk);
    }
  if (thunk == NULL_TREE)
    {
      thunk = build_decl (FUNCTION_DECL, thunk_id, TREE_TYPE (func_decl));
      TREE_READONLY (thunk) = TREE_READONLY (func_decl);
      TREE_THIS_VOLATILE (thunk) = TREE_THIS_VOLATILE (func_decl);
      comdat_linkage (thunk);
      TREE_SET_CODE (thunk, THUNK_DECL);
      DECL_INITIAL (thunk) = function;
      THUNK_DELTA (thunk) = delta;
      THUNK_VCALL_OFFSET (thunk) 
	= vcall_index * TREE_INT_CST_LOW (TYPE_SIZE (vtable_entry_type));
      DECL_EXTERNAL (thunk) = 1;
      DECL_ARTIFICIAL (thunk) = 1;
      /* So that finish_file can write out any thunks that need to be: */
      pushdecl_top_level (thunk);
    }
  return thunk;
}

/* Emit the definition of a C++ multiple inheritance vtable thunk.  */

void
emit_thunk (thunk_fndecl)
     tree thunk_fndecl;
{
  tree function = TREE_OPERAND (DECL_INITIAL (thunk_fndecl), 0);
  int delta = THUNK_DELTA (thunk_fndecl);

  if (TREE_ASM_WRITTEN (thunk_fndecl))
    return;

  TREE_ASM_WRITTEN (thunk_fndecl) = 1;

  TREE_ADDRESSABLE (function) = 1;
  mark_used (function);

  if (current_function_decl)
    abort ();

  TREE_SET_CODE (thunk_fndecl, FUNCTION_DECL);

#ifdef ASM_OUTPUT_MI_THUNK
  if (!flag_syntax_only)
    {
      const char *fnname;
      current_function_decl = thunk_fndecl;
      /* Make sure we build up its RTL before we go onto the
	 temporary obstack.  */
      make_function_rtl (thunk_fndecl);
      DECL_RESULT (thunk_fndecl)
	= build_decl (RESULT_DECL, 0, integer_type_node);
      fnname = XSTR (XEXP (DECL_RTL (thunk_fndecl), 0), 0);
      init_function_start (thunk_fndecl, input_filename, lineno);
      current_function_is_thunk = 1;
      assemble_start_function (thunk_fndecl, fnname);
      ASM_OUTPUT_MI_THUNK (asm_out_file, thunk_fndecl, delta, function);
      assemble_end_function (thunk_fndecl, fnname);
      current_function_decl = 0;
      cfun = 0;
    }
#else /* ASM_OUTPUT_MI_THUNK */
  {
  /* If we don't have the necessary macro for efficient thunks, generate a
     thunk function that just makes a call to the real function.
     Unfortunately, this doesn't work for varargs.  */

    tree a, t;

    if (varargs_function_p (function))
      cp_error ("generic thunk code fails for method `%#D' which uses `...'",
		function);

    /* Set up clone argument trees for the thunk.  */
    t = NULL_TREE;
    for (a = DECL_ARGUMENTS (function); a; a = TREE_CHAIN (a))
      {
	tree x = copy_node (a);
	TREE_CHAIN (x) = t;
	DECL_CONTEXT (x) = thunk_fndecl;
	t = x;
      }
    a = nreverse (t);
    DECL_ARGUMENTS (thunk_fndecl) = a;
    DECL_RESULT (thunk_fndecl) = NULL_TREE;
    DECL_LANG_SPECIFIC (thunk_fndecl) = DECL_LANG_SPECIFIC (function);
    copy_lang_decl (thunk_fndecl);
    DECL_INTERFACE_KNOWN (thunk_fndecl) = 1;
    DECL_NOT_REALLY_EXTERN (thunk_fndecl) = 1;

    start_function (NULL_TREE, thunk_fndecl, NULL_TREE, 
		    SF_DEFAULT | SF_PRE_PARSED);
    store_parm_decls ();
    current_function_is_thunk = 1;

    /* Build up the call to the real function.  */
    t = build_int_2 (delta, -1 * (delta < 0));
    TREE_TYPE (t) = signed_type (sizetype);
    t = fold (build (PLUS_EXPR, TREE_TYPE (a), a, t));
    t = tree_cons (NULL_TREE, t, NULL_TREE);
    for (a = TREE_CHAIN (a); a; a = TREE_CHAIN (a))
      t = tree_cons (NULL_TREE, a, t);
    t = nreverse (t);
    t = build_call (function, t);
    finish_return_stmt (t);

    expand_body (finish_function (lineno, 0));

    /* Don't let the backend defer this function.  */
    if (DECL_DEFER_OUTPUT (thunk_fndecl))
      output_inline_function (thunk_fndecl);
  }
#endif /* ASM_OUTPUT_MI_THUNK */

  TREE_SET_CODE (thunk_fndecl, THUNK_DECL);
}

/* Code for synthesizing methods which have default semantics defined.  */

/* Generate code for default X(X&) constructor.  */

static void
do_build_copy_constructor (fndecl)
     tree fndecl;
{
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));
  tree t;

  if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
    parm = TREE_CHAIN (parm);
  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_INIT_REF (current_class_type)
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (TYPE_HAS_TRIVIAL_INIT_REF (current_class_type))
    {
      t = build (INIT_EXPR, void_type_node, current_class_ref, parm);
      finish_expr_stmt (t);
    }
  else
    {
      tree fields = TYPE_FIELDS (current_class_type);
      int n_bases = CLASSTYPE_N_BASECLASSES (current_class_type);
      tree binfos = TYPE_BINFO_BASETYPES (current_class_type);
      int i;

      /* Initialize all the base-classes.  */
      for (t = CLASSTYPE_VBASECLASSES (current_class_type); t;
	   t = TREE_CHAIN (t))
	current_base_init_list 
	  = tree_cons (BINFO_TYPE (t), parm, current_base_init_list);
      for (i = 0; i < n_bases; ++i)
	{
	  t = TREE_VEC_ELT (binfos, i);
	  if (TREE_VIA_VIRTUAL (t))
	    continue; 

	  current_base_init_list 
	    = tree_cons (BINFO_TYPE (t), parm, current_base_init_list);
	}

      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree init, t;
	  tree field = fields;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  init = parm;
	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;
	      if (VBASE_NAME_P (DECL_NAME (field)))
		continue;

	      /* True for duplicate members.  */
	      if (IDENTIFIER_CLASS_VALUE (DECL_NAME (field)) != field)
		continue;
	    }
	  else if ((t = TREE_TYPE (field)) != NULL_TREE
		   && ANON_AGGR_TYPE_P (t)
		   && TYPE_FIELDS (t) != NULL_TREE)
	    /* Just use the field; anonymous types can't have
	       nontrivial copy ctors or assignment ops.  */;
	  else
	    continue;

	  init = build (COMPONENT_REF, TREE_TYPE (field), init, field);
	  init = build_tree_list (NULL_TREE, init);

	  current_member_init_list
	    = tree_cons (field, init, current_member_init_list);
	}
      current_member_init_list = nreverse (current_member_init_list);
      current_base_init_list = nreverse (current_base_init_list);
      setup_vtbl_ptr ();
    }
}

static void
do_build_assign_ref (fndecl)
     tree fndecl;
{
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));
  tree compound_stmt;

  compound_stmt = begin_compound_stmt (/*has_no_scope=*/0);
  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_ASSIGN_REF (current_class_type)
      && is_empty_class (current_class_type))
    /* Don't copy the padding byte; it might not have been allocated
       if *this is a base subobject.  */;
  else if (TYPE_HAS_TRIVIAL_ASSIGN_REF (current_class_type))
    {
      tree t = build (MODIFY_EXPR, void_type_node, current_class_ref, parm);
      finish_expr_stmt (t);
    }
  else
    {
      tree fields = TYPE_FIELDS (current_class_type);
      int n_bases = CLASSTYPE_N_BASECLASSES (current_class_type);
      tree binfos = TYPE_BINFO_BASETYPES (current_class_type);
      int i;

      for (i = 0; i < n_bases; ++i)
	{
	  tree basetype = BINFO_TYPE (TREE_VEC_ELT (binfos, i));
	  tree p = convert_to_reference
	    (build_reference_type (basetype), parm,
	     CONV_IMPLICIT|CONV_CONST, LOOKUP_COMPLAIN, NULL_TREE);
	  p = convert_from_reference (p);
	  p = build_member_call (basetype, ansi_opname [MODIFY_EXPR],
				 build_tree_list (NULL_TREE, p));
	  finish_expr_stmt (p);
	}
      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree comp, init, t;
	  tree field = fields;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (CP_TYPE_CONST_P (TREE_TYPE (field)))
	    {
	      if (DECL_NAME (field))
		cp_error ("non-static const member `%#D', can't use default assignment operator", field);
	      else
		cp_error ("non-static const member in type `%T', can't use default assignment operator", current_class_type);
	      continue;
	    }
	  else if (TREE_CODE (TREE_TYPE (field)) == REFERENCE_TYPE)
	    {
	      if (DECL_NAME (field))
		cp_error ("non-static reference member `%#D', can't use default assignment operator", field);
	      else
		cp_error ("non-static reference member in type `%T', can't use default assignment operator", current_class_type);
	      continue;
	    }

	  comp = current_class_ref;
	  init = parm;

	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;
	      if (VBASE_NAME_P (DECL_NAME (field)))
		continue;

	      /* True for duplicate members.  */
	      if (IDENTIFIER_CLASS_VALUE (DECL_NAME (field)) != field)
		continue;
	    }
	  else if ((t = TREE_TYPE (field)) != NULL_TREE
		   && ANON_AGGR_TYPE_P (t)
		   && TYPE_FIELDS (t) != NULL_TREE)
	    /* Just use the field; anonymous types can't have
	       nontrivial copy ctors or assignment ops.  */;
	  else
	    continue;

	  comp = build (COMPONENT_REF, TREE_TYPE (field), comp, field);
	  init = build (COMPONENT_REF, TREE_TYPE (field), init, field);

	  finish_expr_stmt (build_modify_expr (comp, NOP_EXPR, init));
	}
    }
  finish_return_stmt (current_class_ref);
  finish_compound_stmt (/*has_no_scope=*/0, compound_stmt);
}

void
synthesize_method (fndecl)
     tree fndecl;
{
  int nested = (current_function_decl != NULL_TREE);
  tree context = decl_function_context (fndecl);
  int need_body = 1;

  if (at_eof)
    import_export_decl (fndecl);

  if (! context)
    push_to_top_level ();
  else if (nested)
    push_function_context_to (context);

  /* Put the function definition at the position where it is needed,
     rather than within the body of the class.  That way, an error
     during the generation of the implicit body points at the place
     where the attempt to generate the function occurs, giving the
     user a hint as to why we are attempting to generate the
     function. */
  DECL_SOURCE_LINE (fndecl) = lineno;
  DECL_SOURCE_FILE (fndecl) = input_filename;

  interface_unknown = 1;
  start_function (NULL_TREE, fndecl, NULL_TREE, SF_DEFAULT | SF_PRE_PARSED);
  store_parm_decls ();
  clear_last_expr ();

  if (DECL_NAME (fndecl) == ansi_opname[MODIFY_EXPR])
    {
      do_build_assign_ref (fndecl);
      need_body = 0;
    }
  else if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (fndecl)))
    setup_vtbl_ptr ();
  else
    {
      tree arg_chain = FUNCTION_ARG_CHAIN (fndecl);
      if (DECL_CONSTRUCTOR_FOR_VBASE_P (fndecl))
	arg_chain = TREE_CHAIN (arg_chain);
      if (arg_chain != void_list_node)
	do_build_copy_constructor (fndecl);
      else if (TYPE_NEEDS_CONSTRUCTING (current_class_type))
	setup_vtbl_ptr ();
    }

  /* If we haven't yet generated the body of the function, just
     generate an empty compound statement.  */
  if (need_body)
    {
      tree compound_stmt;
      compound_stmt = begin_compound_stmt (/*has_no_scope=*/0);
      finish_compound_stmt (/*has_no_scope=*/0, compound_stmt);
    }

  expand_body (finish_function (lineno, 0));

  extract_interface_info ();
  if (! context)
    pop_from_top_level ();
  else if (nested)
    pop_function_context_from (context);
}
