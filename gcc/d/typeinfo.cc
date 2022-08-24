/* typeinfo.cc -- D runtime type identification.
   Copyright (C) 2013-2022 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/aggregate.h"
#include "dmd/enum.h"
#include "dmd/errors.h"
#include "dmd/expression.h"
#include "dmd/globals.h"
#include "dmd/identifier.h"
#include "dmd/module.h"
#include "dmd/mtype.h"
#include "dmd/scope.h"
#include "dmd/template.h"
#include "dmd/target.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "toplev.h"
#include "stor-layout.h"

#include "d-tree.h"
#include "d-frontend.h"
#include "d-target.h"


/* D returns type information to the user as TypeInfo class objects, and can
   be retrieved for any type using `typeid()'.  We also use type information
   to implement many runtime library helpers, including `new', `delete', most
   dynamic array operations, and all associative array operations.

   Type information for a particular type is indicated with an ABI defined
   structure derived from TypeInfo.  This would all be very straight forward,
   but for the fact that the runtime library provides the definitions of the
   TypeInfo structure and the ABI defined derived classes in `object.d', as
   well as having specific implementations of TypeInfo for built-in types
   in `rt/typeinfo`.  We cannot build declarations of these directly in the
   compiler, but we need to layout objects of their type.

   To get around this, we define layout compatible POD-structs and generate the
   appropriate initializations for them.  When we have to provide a TypeInfo to
   the user, we cast the internal compiler type to TypeInfo.

   It is only required that TypeInfo has a definition in `object.d'.  It could
   happen that we are generating a type information for a TypeInfo object that
   has no declaration.  We however only need the addresses of such incomplete
   TypeInfo objects for static initialization.  */

enum tinfo_kind
{
  TK_TYPEINFO_TYPE,		/* object.TypeInfo  */
  TK_CLASSINFO_TYPE,		/* object.TypeInfo_Class  */
  TK_INTERFACE_TYPE,		/* object.TypeInfo_Interface  */
  TK_STRUCT_TYPE,		/* object.TypeInfo_Struct  */
  TK_POINTER_TYPE,		/* object.TypeInfo_Pointer  */
  TK_ARRAY_TYPE,		/* object.TypeInfo_Array  */
  TK_STATICARRAY_TYPE,		/* object.TypeInfo_StaticArray  */
  TK_ASSOCIATIVEARRAY_TYPE,	/* object.TypeInfo_AssociativeArray  */
  TK_VECTOR_TYPE,		/* object.TypeInfo_Vector  */
  TK_ENUMERAL_TYPE,		/* object.TypeInfo_Enum  */
  TK_FUNCTION_TYPE,		/* object.TypeInfo_Function  */
  TK_DELEGATE_TYPE,		/* object.TypeInfo_Delegate  */
  TK_TYPELIST_TYPE,		/* object.TypeInfo_Tuple  */
  TK_CONST_TYPE,		/* object.TypeInfo_Const  */
  TK_IMMUTABLE_TYPE,		/* object.TypeInfo_Invariant  */
  TK_SHARED_TYPE,		/* object.TypeInfo_Shared  */
  TK_INOUT_TYPE,		/* object.TypeInfo_Inout  */
  TK_CPPTI_TYPE,		/* object.__cpp_type_info_ptr  */
  TK_END
};

/* An array of all internal TypeInfo derived types we need.
   The TypeInfo and ClassInfo types are created early, the
   remainder are generated as needed.  */

static GTY(()) tree tinfo_types[TK_END];

/* Return the kind of TypeInfo used to describe TYPE.  */

static tinfo_kind
get_typeinfo_kind (Type *type)
{
  /* Check head shared/const modifiers first.  */
  if (type->isShared ())
    return TK_SHARED_TYPE;
  else if (type->isConst ())
    return TK_CONST_TYPE;
  else if (type->isImmutable ())
    return TK_IMMUTABLE_TYPE;
  else if (type->isWild ())
    return TK_INOUT_TYPE;

  switch (type->ty)
    {
    case TY::Tpointer:
      return TK_POINTER_TYPE;

    case TY::Tarray:
      return TK_ARRAY_TYPE;

    case TY::Tsarray:
      return TK_STATICARRAY_TYPE;

    case TY::Taarray:
      return TK_ASSOCIATIVEARRAY_TYPE;

    case TY::Tstruct:
      return TK_STRUCT_TYPE;

    case TY::Tvector:
      return TK_VECTOR_TYPE;

    case TY::Tenum:
      return TK_ENUMERAL_TYPE;

    case TY::Tfunction:
      return TK_FUNCTION_TYPE;

    case TY::Tdelegate:
      return TK_DELEGATE_TYPE;

    case TY::Ttuple:
      return TK_TYPELIST_TYPE;

    case TY::Tclass:
      if (type->isTypeClass ()->sym->isInterfaceDeclaration ())
	return TK_INTERFACE_TYPE;
      else
	return TK_CLASSINFO_TYPE;

    default:
      return TK_TYPEINFO_TYPE;
    }
}

/* Generate the RECORD_TYPE containing the data layout of a TypeInfo derivative
   as used by the runtime.  This layout must be consistent with that defined in
   the `object.d' module.  */

static void
make_internal_typeinfo (tinfo_kind tk, Identifier *ident, ...)
{
  va_list ap;

  va_start (ap, ident);

  /* First two fields are from the TypeInfo base class.
     Note, finish_builtin_struct() expects these fields in reverse order.  */
  tree fields = create_field_decl (ptr_type_node, NULL, 1, 1);
  DECL_CHAIN (fields) = create_field_decl (vtbl_ptr_type_node, NULL, 1, 1);

  /* Now add the derived fields.  */
  tree field_type = va_arg (ap, tree);
  while (field_type != NULL_TREE)
    {
      tree field = create_field_decl (field_type, NULL, 1, 1);
      DECL_CHAIN (field) = fields;
      fields = field;
      field_type = va_arg (ap, tree);
    }

  /* Create the TypeInfo type.  */
  tree type = make_node (RECORD_TYPE);
  TYPE_ARTIFICIAL (type) = 1;
  finish_builtin_struct (type, ident->toChars (), fields, NULL_TREE);

  tinfo_types[tk] = type;

  va_end (ap);
}

/* Reference to the `object` module, where all TypeInfo is defined.  */

static Module *object_module;

/* Helper for create_frontend_tinfo_types.  Creates a typeinfo class
   declaration incase one wasn't supplied by reading `object.d'.  */

static void
make_frontend_typeinfo (Identifier *ident, ClassDeclaration *base = NULL)
{
  if (!base)
    base = Type::dtypeinfo;

  gcc_assert (object_module);

  /* Create object module in order to complete the semantic.  */
  if (!object_module->_scope)
    object_module->importAll (NULL);

  /* Object class doesn't exist, create a stub one that will cause an error if
     used.  */
  Loc loc = (object_module->md) ? object_module->md->loc : object_module->loc;
  if (!base)
    {
      if (!ClassDeclaration::object)
	{
	  ClassDeclaration *object
	    = ClassDeclaration::create (loc, Identifier::idPool ("Object"),
					NULL, NULL, true);
	  object->parent = object_module;
	  object->members = d_gc_malloc<Dsymbols> ();
	  object->storage_class |= STCtemp;
	}

      base = ClassDeclaration::object;
    }

  /* Assignment of global typeinfo variables is managed by the ClassDeclaration
     constructor, so only need to new the declaration here.  */
  ClassDeclaration *tinfo = ClassDeclaration::create (loc, ident, NULL, NULL,
						      true);
  tinfo->parent = object_module;
  tinfo->members = d_gc_malloc<Dsymbols> ();
  dsymbolSemantic (tinfo, object_module->_scope);
  tinfo->baseClass = base;
  /* This is a compiler generated class, and shouldn't be mistaken for being
     the type declared in the runtime library.  */
  tinfo->storage_class |= STCtemp;
}

/* Make sure the required builtin types exist for generating the TypeInfo
   variable definitions.  */

void
create_tinfo_types (Module *mod)
{
  /* Already generated internal types for the object module.  */
  if (object_module != NULL)
    return;

  /* Build the internal TypeInfo and ClassInfo types.
     See TypeInfoVisitor for documentation of field layout.  */
  make_internal_typeinfo (TK_TYPEINFO_TYPE, Identifier::idPool ("TypeInfo"),
			  NULL);

  make_internal_typeinfo (TK_CLASSINFO_TYPE,
			  Identifier::idPool ("TypeInfo_Class"),
			  array_type_node, array_type_node, array_type_node,
			  array_type_node, ptr_type_node, ptr_type_node,
			  ptr_type_node, d_uint_type, ptr_type_node,
			  array_type_node, ptr_type_node, ptr_type_node, NULL);

  object_module = mod;
}

/* Same as create_tinfo_types, but builds all front-end TypeInfo variable
   definitions.  */

static void
create_frontend_tinfo_types (void)
{
  /* If there's no object module, then neither can there be TypeInfo.  */
  if (object_module == NULL)
    return;

  /* Create all frontend TypeInfo classes declarations.  We rely on all
     existing, even if only just as stubs.  */
  if (!Type::dtypeinfo)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo"),
			    ClassDeclaration::object);

  if (!Type::typeinfoclass)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Class"));

  if (!Type::typeinfointerface)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Interface"));

  if (!Type::typeinfostruct)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Struct"));

  if (!Type::typeinfopointer)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Pointer"));

  if (!Type::typeinfoarray)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Array"));

  if (!Type::typeinfostaticarray)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_StaticArray"));

  if (!Type::typeinfoassociativearray)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_AssociativeArray"));

  if (!Type::typeinfoenum)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Enum"));

  if (!Type::typeinfofunction)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Function"));

  if (!Type::typeinfodelegate)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Delegate"));

  if (!Type::typeinfotypelist)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Tuple"));

  if (!Type::typeinfoconst)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Const"));

  if (!Type::typeinfoinvariant)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Invariant"),
			    Type::typeinfoconst);

  if (!Type::typeinfoshared)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Shared"),
			    Type::typeinfoconst);

  if (!Type::typeinfowild)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Wild"),
			    Type::typeinfoconst);

  if (!Type::typeinfovector)
    make_frontend_typeinfo (Identifier::idPool ("TypeInfo_Vector"));

  if (!ClassDeclaration::cpp_type_info_ptr)
    make_frontend_typeinfo (Identifier::idPool ("__cpp_type_info_ptr"),
			    ClassDeclaration::object);
}

/* Return true if TypeInfo class TINFO is available in the runtime library.  */

bool
have_typeinfo_p (ClassDeclaration *tinfo)
{
  /* Run-time typeinfo disabled on command line.  */
  if (!global.params.useTypeInfo)
    return false;

  /* Can't layout TypeInfo if type is not declared, or is an opaque
     declaration in the object module.  */
  if (!tinfo || !tinfo->members)
    return false;

  /* Typeinfo is compiler-generated.  */
  if (tinfo->storage_class & STCtemp)
    return false;

  return true;
}

/* Implements the visitor interface to build the TypeInfo layout of all
   TypeInfoDeclaration AST classes emitted from the D Front-end.
   All visit methods accept one parameter D, which holds the frontend AST
   of the TypeInfo class.  They also don't return any value, instead the
   generated symbol is cached internally and returned from the caller.  */

class TypeInfoVisitor : public Visitor
{
  using Visitor::visit;

  tree decl_;
  vec<constructor_elt, va_gc> *init_;

  /* Build an internal comdat symbol for the manifest constant VALUE, so that
     its address can be taken.  */

  tree internal_reference (tree value)
  {
    /* Use the typeinfo decl name as a prefix for the internal symbol.  */
    const char *prefix = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (this->decl_));
    tree decl = build_artificial_decl (TREE_TYPE (value), value, prefix);

    /* The internal pointer reference should be public, but not visible outside
       the compilation unit.  */
    DECL_EXTERNAL (decl) = 0;
    TREE_PUBLIC (decl) = 1;
    DECL_VISIBILITY (decl) = VISIBILITY_INTERNAL;
    set_linkage_for_decl (decl);
    d_pushdecl (decl);

    return decl;
  }

  /* Add VALUE to the constructor values list.  */

  void layout_field (tree value)
  {
    CONSTRUCTOR_APPEND_ELT (this->init_, NULL_TREE, value);
  }

  /* Write out STR as a static D string literal.  */

  void layout_string (const char *str)
  {
    unsigned len = strlen (str);
    tree value = build_string (len, str);

    TREE_TYPE (value) = make_array_type (Type::tchar, len);
    TREE_CONSTANT (value) = 1;
    TREE_READONLY (value) = 1;
    TREE_STATIC (value) = 1;

    /* Taking the address, so assign the literal to a static var.  */
    tree decl = this->internal_reference (value);
    TREE_READONLY (decl) = 1;

    value = d_array_value (build_ctype (Type::tchar->arrayOf ()),
			   size_int (len), build_address (decl));
    this->layout_field (value);
  }

  /* Write out the __vptr and optionally __monitor fields of class CD.  */

  void layout_base (ClassDeclaration *cd)
  {
    gcc_assert (cd != NULL);

    if (have_typeinfo_p (cd))
      this->layout_field (build_address (get_vtable_decl (cd)));
    else
      this->layout_field (null_pointer_node);

    if (cd->hasMonitor ())
      this->layout_field (null_pointer_node);
  }

  /* Write out the interfaces field of class CD.
     Returns the array of interfaces that the field is pointing to.  */

  tree layout_interfaces (ClassDeclaration *cd)
  {
    size_t offset = int_size_in_bytes (tinfo_types[TK_CLASSINFO_TYPE]);
    tree csym = build_address (get_classinfo_decl (cd));

    /* Put out the offset to where vtblInterfaces are written.  */
    tree value = d_array_value (array_type_node,
				size_int (cd->vtblInterfaces->length),
				build_offset (csym, size_int (offset)));
    this->layout_field (value);

    /* Internally, the compiler sees Interface as:
	void*[4] interface;

       The run-time layout of Interface is:
	TypeInfo_Class classinfo;
	void*[] vtbl;
	size_t offset;  */
    vec<constructor_elt, va_gc> *elms = NULL;

    for (size_t i = 0; i < cd->vtblInterfaces->length; i++)
      {
	BaseClass *b = (*cd->vtblInterfaces)[i];
	ClassDeclaration *id = b->sym;
	vec<constructor_elt, va_gc> *v = NULL;

	/* Fill in the vtbl[].  */
	if (!cd->isInterfaceDeclaration ())
	  b->fillVtbl (cd, &b->vtbl, 1);

	/* ClassInfo for the interface.  */
	value = build_address (get_classinfo_decl (id));
	CONSTRUCTOR_APPEND_ELT (v, size_int (0), value);

	if (!cd->isInterfaceDeclaration ())
	  {
	    /* The vtable of the interface length and ptr.  */
	    unsigned voffset = base_vtable_offset (cd, b);
	    gcc_assert (voffset != 0u);
	    value = build_offset (csym, size_int (voffset));

	    CONSTRUCTOR_APPEND_ELT (v, size_int (1),
				    size_int (id->vtbl.length));
	    CONSTRUCTOR_APPEND_ELT (v, size_int (2), value);
	  }

	/* The `this' offset.  */
	CONSTRUCTOR_APPEND_ELT (v, size_int (3), size_int (b->offset));

	/* Add to the array of interfaces.  */
	value = build_constructor (vtbl_interface_type_node, v);
	CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
      }

    tree domain = size_int (cd->vtblInterfaces->length - 1);
    tree arrtype = build_array_type (vtbl_interface_type_node,
				     build_index_type (domain));
    return build_constructor (arrtype, elms);
  }

  /* Write out the interfacing vtable[] of base class BCD that will be accessed
     from the overriding class CD.  If both are the same class, then this will
     be its own vtable.  INDEX is the offset in the interfaces array of the
     base class where the Interface reference can be found.
     This must be mirrored with base_vtable_offset().  */

  void layout_base_vtable (ClassDeclaration *cd, ClassDeclaration *bcd,
			   size_t index)
  {
    BaseClass *bs = (*bcd->vtblInterfaces)[index];
    ClassDeclaration *id = bs->sym;
    vec<constructor_elt, va_gc> *elms = NULL;
    FuncDeclarations bvtbl;

    if (id->vtbl.length == 0 || base_vtable_offset (cd, bs) == ~0u)
      return;

    /* Fill bvtbl with the functions we want to put out.  */
    if (cd != bcd && !bs->fillVtbl (cd, &bvtbl, 0))
      return;

    /* First entry is struct Interface reference.  */
    if (id->vtblOffset ())
      {
	size_t offset = int_size_in_bytes (tinfo_types[TK_CLASSINFO_TYPE]);
	offset += (index * int_size_in_bytes (vtbl_interface_type_node));
	tree value = build_offset (build_address (get_classinfo_decl (bcd)),
				   size_int (offset));
	CONSTRUCTOR_APPEND_ELT (elms, size_zero_node, value);
      }

    for (size_t i = id->vtblOffset () ? 1 : 0; i < id->vtbl.length; i++)
      {
	FuncDeclaration *fd = (cd == bcd) ? bs->vtbl[i] : bvtbl[i];
	if (fd != NULL)
	  {
	    tree value = build_address (make_thunk (fd, bs->offset));
	    CONSTRUCTOR_APPEND_ELT (elms, size_int (i), value);
	  }
      }

    tree vtbldomain = build_index_type (size_int (id->vtbl.length - 1));
    tree vtbltype = build_array_type (vtable_entry_type, vtbldomain);
    tree value = build_constructor (vtbltype, elms);
    this->layout_field (value);
  }


public:
  TypeInfoVisitor (tree decl)
  {
    this->decl_ = decl;
    this->init_ = NULL;
  }

  /* Return the completed constructor for the TypeInfo record.  */

  tree result (void)
  {
    return build_struct_literal (TREE_TYPE (this->decl_), this->init_);
  }

  /* Layout of TypeInfo is:
	void **__vptr;
	void *__monitor;  */

  void visit (TypeInfoDeclaration *) final override
  {
    /* The vtable for TypeInfo.  */
    this->layout_base (Type::dtypeinfo);
  }

  /* Layout of TypeInfo_Const is:
	void **__vptr;
	void *__monitor;
	TypeInfo base;  */

  void visit (TypeInfoConstDeclaration *d) final override
  {
    Type *tm = d->tinfo->mutableOf ();
    tm = tm->merge2 ();

    /* The vtable for TypeInfo_Const.  */
    this->layout_base (Type::typeinfoconst);

    /* TypeInfo for the mutable type.  */
    this->layout_field (build_typeinfo (d->loc, tm));
  }

  /* Layout of TypeInfo_Immutable is:
	void **__vptr;
	void *__monitor;
	TypeInfo base;  */

  void visit (TypeInfoInvariantDeclaration *d) final override
  {
    Type *tm = d->tinfo->mutableOf ();
    tm = tm->merge2 ();

    /* The vtable for TypeInfo_Invariant.  */
    this->layout_base (Type::typeinfoinvariant);

    /* TypeInfo for the mutable type.  */
    this->layout_field (build_typeinfo (d->loc, tm));
  }

  /* Layout of TypeInfo_Shared is:
	void **__vptr;
	void *__monitor;
	TypeInfo base;  */

  void visit (TypeInfoSharedDeclaration *d) final override
  {
    Type *tm = d->tinfo->unSharedOf ();
    tm = tm->merge2 ();

    /* The vtable for TypeInfo_Shared.  */
    this->layout_base (Type::typeinfoshared);

    /* TypeInfo for the unshared type.  */
    this->layout_field (build_typeinfo (d->loc, tm));
  }

  /* Layout of TypeInfo_Inout is:
	void **__vptr;
	void *__monitor;
	TypeInfo base;  */

  void visit (TypeInfoWildDeclaration *d) final override
  {
    Type *tm = d->tinfo->mutableOf ();
    tm = tm->merge2 ();

    /* The vtable for TypeInfo_Inout.  */
    this->layout_base (Type::typeinfowild);

    /* TypeInfo for the mutable type.  */
    this->layout_field (build_typeinfo (d->loc, tm));
  }

  /* Layout of TypeInfo_Enum is:
	void **__vptr;
	void *__monitor;
	TypeInfo base;
	string name;
	void[] m_init;  */

  void visit (TypeInfoEnumDeclaration *d) final override
  {
    TypeEnum *ti = d->tinfo->isTypeEnum ();
    EnumDeclaration *ed = ti->sym;

    /* The vtable for TypeInfo_Enum.  */
    this->layout_base (Type::typeinfoenum);

    /* TypeInfo for enum members.  */
    tree memtype = (ed->memtype) ? build_typeinfo (d->loc, ed->memtype)
      : null_pointer_node;
    this->layout_field (memtype);

    /* Name of the enum declaration.  */
    this->layout_string (ed->toPrettyChars ());

    /* Default initializer for enum.  */
    if (ed->members && !d->tinfo->isZeroInit ())
      {
	tree length = size_int (ed->type->size ());
	tree ptr = build_address (enum_initializer_decl (ed));
	this->layout_field (d_array_value (array_type_node, length, ptr));
      }
    else
      this->layout_field (null_array_node);
  }

  /* Layout of TypeInfo_Pointer is:
	void **__vptr;
	void *__monitor;
	TypeInfo m_next;  */

  void visit (TypeInfoPointerDeclaration *d) final override
  {
    TypePointer *ti = d->tinfo->isTypePointer ();

    /* The vtable for TypeInfo_Pointer.  */
    this->layout_base (Type::typeinfopointer);

    /* TypeInfo for pointer-to type.  */
    this->layout_field (build_typeinfo (d->loc, ti->next));
  }

  /* Layout of TypeInfo_Array is:
	void **__vptr;
	void *__monitor;
	TypeInfo value;  */

  void visit (TypeInfoArrayDeclaration *d) final override
  {
    TypeDArray *ti = d->tinfo->isTypeDArray ();

    /* The vtable for TypeInfo_Array.  */
    this->layout_base (Type::typeinfoarray);

    /* TypeInfo for array of type.  */
    this->layout_field (build_typeinfo (d->loc, ti->next));
  }

  /* Layout of TypeInfo_StaticArray is:
	void **__vptr;
	void *__monitor;
	TypeInfo value;
	size_t len;  */

  void visit (TypeInfoStaticArrayDeclaration *d) final override
  {
    TypeSArray *ti = d->tinfo->isTypeSArray ();

    /* The vtable for TypeInfo_StaticArray.  */
    this->layout_base (Type::typeinfostaticarray);

    /* TypeInfo for array of type.  */
    this->layout_field (build_typeinfo (d->loc, ti->next));

    /* Static array length.  */
    this->layout_field (size_int (ti->dim->toInteger ()));
  }

  /* Layout of TypeInfo_AssociativeArray is:
	void **__vptr;
	void *__monitor;
	TypeInfo value;
	TypeInfo key;  */

  void visit (TypeInfoAssociativeArrayDeclaration *d) final override
  {
    TypeAArray *ti = d->tinfo->isTypeAArray ();

    /* The vtable for TypeInfo_AssociativeArray.  */
    this->layout_base (Type::typeinfoassociativearray);

    /* TypeInfo for value of type.  */
    this->layout_field (build_typeinfo (d->loc, ti->next));

    /* TypeInfo for index of type.  */
    this->layout_field (build_typeinfo (d->loc, ti->index));
  }

  /* Layout of TypeInfo_Vector is:
	void **__vptr;
	void *__monitor;
	TypeInfo base;  */

  void visit (TypeInfoVectorDeclaration *d) final override
  {
    TypeVector *ti = d->tinfo->isTypeVector ();

    /* The vtable for TypeInfo_Vector.  */
    this->layout_base (Type::typeinfovector);

    /* TypeInfo for equivalent static array.  */
    this->layout_field (build_typeinfo (d->loc, ti->basetype));
  }

  /* Layout of TypeInfo_Function is:
	void **__vptr;
	void *__monitor;
	TypeInfo next;
	string deco;  */

  void visit (TypeInfoFunctionDeclaration *d) final override
  {
    TypeFunction *ti = d->tinfo->isTypeFunction ();
    gcc_assert (ti->deco != NULL);

    /* The vtable for TypeInfo_Function.  */
    this->layout_base (Type::typeinfofunction);

    /* TypeInfo for function return value.  */
    this->layout_field (build_typeinfo (d->loc, ti->next));

    /* Mangled name of function declaration.  */
    this->layout_string (d->tinfo->deco);
  }

  /* Layout of TypeInfo_Delegate is:
	void **__vptr;
	void *__monitor;
	TypeInfo next;
	string deco;  */

  void visit (TypeInfoDelegateDeclaration *d) final override
  {
    TypeDelegate *ti = d->tinfo->isTypeDelegate ();
    gcc_assert (ti->deco != NULL);

    /* The vtable for TypeInfo_Delegate.  */
    this->layout_base (Type::typeinfodelegate);

    /* TypeInfo for delegate return value.  */
    this->layout_field (build_typeinfo (d->loc, ti->next));

    /* Mangled name of delegate declaration.  */
    this->layout_string (d->tinfo->deco);
  }

  /* Layout of ClassInfo/TypeInfo_Class is:
	void **__vptr;
	void *__monitor;
	byte[] m_init;
	string name;
	void*[] vtbl;
	Interface[] interfaces;
	TypeInfo_Class base;
	void *destructor;
	void function(Object) classInvariant;
	ClassFlags m_flags;
	void *deallocator;
	OffsetTypeInfo[] m_offTi;
	void function(Object) defaultConstructor;
	immutable(void)* m_RTInfo;

     Information relating to interfaces, and their vtables are laid out
     immediately after the named fields, if there is anything to write.  */

  void visit (TypeInfoClassDeclaration *d) final override
  {
    TypeClass *ti = d->tinfo->isTypeClass ();
    ClassDeclaration *cd = ti->sym;

    /* The vtable for ClassInfo.  */
    this->layout_base (Type::typeinfoclass);

    if (!cd->members)
      return;

    tree interfaces = NULL_TREE;

    if (!cd->isInterfaceDeclaration ())
      {
	/* Default initializer for class.  */
	tree init = aggregate_initializer_decl (cd);
	tree value = d_array_value (array_type_node, size_int (cd->structsize),
				    build_address (init));
	this->layout_field (value);

	/* Name of the class declaration.  */
	const char *name = cd->ident->toChars ();
	if (!(strlen (name) > 9 && memcmp (name, "TypeInfo_", 9) == 0))
	  name = cd->toPrettyChars (true);
	this->layout_string (name);

	/* The vtable of the class declaration.  */
	value = d_array_value (array_type_node, size_int (cd->vtbl.length),
			       build_address (get_vtable_decl (cd)));
	this->layout_field (value);

	/* Array of base interfaces that have their own vtable.  */
	if (cd->vtblInterfaces->length)
	  interfaces = this->layout_interfaces (cd);
	else
	  this->layout_field (null_array_node);

	/* TypeInfo_Class base;  */
	tree base = (cd->baseClass)
	  ? build_address (get_classinfo_decl (cd->baseClass))
	  : null_pointer_node;
	this->layout_field (base);

	/* void *destructor;  */
	tree dtor = (cd->tidtor) ? build_address (get_symbol_decl (cd->tidtor))
	  : null_pointer_node;
	this->layout_field (dtor);

	/* void function(Object) classInvariant;  */
	tree inv = (cd->inv) ? build_address (get_symbol_decl (cd->inv))
	  : null_pointer_node;
	this->layout_field (inv);

	/* ClassFlags m_flags;  */
	int flags = ClassFlags::hasOffTi;
	if (cd->isCOMclass ())
	  flags |= ClassFlags::isCOMclass;

	if (cd->isCPPclass ())
	  flags |= ClassFlags::isCPPclass;

	flags |= ClassFlags::hasGetMembers;
	flags |= ClassFlags::hasTypeInfo;

	if (cd->ctor)
	  flags |= ClassFlags::hasCtor;

	for (ClassDeclaration *bcd = cd; bcd; bcd = bcd->baseClass)
	  {
	    if (bcd->dtor)
	      {
		flags |= ClassFlags::hasDtor;
		break;
	      }
	  }

	if (cd->isAbstract ())
	  flags |= ClassFlags::isAbstract;

	for (ClassDeclaration *bcd = cd; bcd; bcd = bcd->baseClass)
	  {
	    if (!bcd->members)
	      continue;

	    for (size_t i = 0; i < bcd->members->length; i++)
	      {
		Dsymbol *sm = (*bcd->members)[i];
		if (sm->hasPointers ())
		  goto Lhaspointers;
	      }
	  }

	flags |= ClassFlags::noPointers;

    Lhaspointers:
	this->layout_field (build_integer_cst (flags, d_uint_type));

	/* void *deallocator;  */
	this->layout_field (null_pointer_node);

	/* OffsetTypeInfo[] m_offTi;  (not implemented)  */
	this->layout_field (null_array_node);

	/* void function(Object) defaultConstructor;  */
	if (cd->defaultCtor && !(cd->defaultCtor->storage_class & STCdisable))
	  {
	    tree dctor = get_symbol_decl (cd->defaultCtor);
	    this->layout_field (build_address (dctor));
	  }
	else
	  this->layout_field (null_pointer_node);

	/* immutable(void)* m_RTInfo;  */
	if (cd->getRTInfo)
	  this->layout_field (build_expr (cd->getRTInfo, true));
	else if (!(flags & ClassFlags::noPointers))
	  this->layout_field (size_one_node);
	else
	  this->layout_field (null_pointer_node);
      }
    else
      {
	/* No initializer for interface.  */
	this->layout_field (null_array_node);

	/* Name of the interface declaration.  */
	this->layout_string (cd->toPrettyChars (true));

	/* No vtable for interface declaration.  */
	this->layout_field (null_array_node);

	/* Array of base interfaces that have their own vtable.  */
	if (cd->vtblInterfaces->length)
	  interfaces = this->layout_interfaces (cd);
	else
	  this->layout_field (null_array_node);

	/* TypeInfo_Class base;
	   void *destructor;
	   void function(Object) classInvariant;  */
	this->layout_field (null_pointer_node);
	this->layout_field (null_pointer_node);
	this->layout_field (null_pointer_node);

	/* ClassFlags m_flags;  */
	int flags = ClassFlags::hasOffTi;
	flags |= ClassFlags::hasTypeInfo;
	if (cd->isCOMinterface ())
	  flags |= ClassFlags::isCOMclass;

	this->layout_field (build_integer_cst (flags, d_uint_type));

	/* void *deallocator;
	   OffsetTypeInfo[] m_offTi;  (not implemented)
	   void function(Object) defaultConstructor;  */
	this->layout_field (null_pointer_node);
	this->layout_field (null_array_node);
	this->layout_field (null_pointer_node);

	/* immutable(void)* m_RTInfo;  */
	if (cd->getRTInfo)
	  this->layout_field (build_expr (cd->getRTInfo, true));
	else
	  this->layout_field (null_pointer_node);
      }

    /* Put out array of Interfaces.  */
    if (interfaces != NULL_TREE)
      this->layout_field (interfaces);

    if (!cd->isInterfaceDeclaration ())
      {
	/* Put out this class' interface vtables[].  */
	for (size_t i = 0; i < cd->vtblInterfaces->length; i++)
	  this->layout_base_vtable (cd, cd, i);

	/* Put out the overriding interface vtables[].  */
	for (ClassDeclaration *bcd = cd->baseClass; bcd; bcd = bcd->baseClass)
	  {
	    for (size_t i = 0; i < bcd->vtblInterfaces->length; i++)
	      this->layout_base_vtable (cd, bcd, i);
	  }
      }
  }

  /* Layout of TypeInfo_Interface is:
	void **__vptr;
	void *__monitor;
	TypeInfo_Class info;  */

  void visit (TypeInfoInterfaceDeclaration *d) final override
  {
    TypeClass *ti = d->tinfo->isTypeClass ();

    if (!ti->sym->vclassinfo)
      ti->sym->vclassinfo = TypeInfoClassDeclaration::create (ti);

    /* The vtable for TypeInfo_Interface.  */
    this->layout_base (Type::typeinfointerface);

    /* TypeInfo for class inheriting the interface.  */
    tree tidecl = get_typeinfo_decl (ti->sym->vclassinfo);
    this->layout_field (build_address (tidecl));
  }

  /* Layout of TypeInfo_Struct is:
	void **__vptr;
	void *__monitor;
	string mangledName;
	void[] m_init;
	hash_t function(in void*) xtoHash;
	bool function(in void*, in void*) xopEquals;
	int function(in void*, in void*) xopCmp;
	string function(const(void)*) xtoString;
	StructFlags m_flags;
	void function(void*) xdtor;
	void function(void*) xpostblit;
	uint m_align;
	immutable(void)* xgetRTInfo;  */

  void visit (TypeInfoStructDeclaration *d) final override
  {
    TypeStruct *ti = d->tinfo->isTypeStruct ();
    StructDeclaration *sd = ti->sym;

    /* The vtable for TypeInfo_Struct.  */
    this->layout_base (Type::typeinfostruct);

    if (!sd->members)
      return;

    /* Mangled name of the struct declaration.  */
    this->layout_string (ti->deco);

    /* Default initializer for struct.  */
    tree ptr = (sd->zeroInit ()) ? null_pointer_node
      : build_address (aggregate_initializer_decl (sd));
    this->layout_field (d_array_value (array_type_node,
				       size_int (sd->structsize), ptr));

    /* hash_t function (in void*) xtoHash;  */
    tree xhash = (sd->xhash) ? build_address (get_symbol_decl (sd->xhash))
      : null_pointer_node;
    this->layout_field (xhash);

    if (sd->xhash)
      {
	TypeFunction *tf = sd->xhash->type->toTypeFunction ();
	if (!tf->isnothrow () || tf->trust == TRUST::system)
	  {
	    warning (sd->xhash->loc, "toHash() must be declared as "
		     "extern (D) size_t toHash() const nothrow @safe, "
		     "not %s", tf->toChars ());
	  }
      }

    /* bool function(in void*, in void*) xopEquals;  */
    tree xeq = (sd->xeq) ? build_address (get_symbol_decl (sd->xeq))
      : null_pointer_node;
    this->layout_field (xeq);

    /* int function(in void*, in void*) xopCmp;  */
    tree xcmp = (sd->xcmp) ? build_address (get_symbol_decl (sd->xcmp))
      : null_pointer_node;
    this->layout_field (xcmp);

    /* string function(const(void)*) xtoString;  */
    FuncDeclaration *fdx = search_toString (sd);
    if (fdx)
      this->layout_field (build_address (get_symbol_decl (fdx)));
    else
      this->layout_field (null_pointer_node);

    /* StructFlags m_flags;  */
    int m_flags = StructFlags::none;
    if (ti->hasPointers ())
      m_flags |= StructFlags::hasPointers;
    this->layout_field (build_integer_cst (m_flags, d_uint_type));

    /* void function(void*) xdtor;  */
    tree dtor = (sd->tidtor) ? build_address (get_symbol_decl (sd->tidtor))
      : null_pointer_node;
    this->layout_field (dtor);

    /* void function(void*) xpostblit;  */
    if (sd->postblit && !(sd->postblit->storage_class & STCdisable))
      this->layout_field (build_address (get_symbol_decl (sd->postblit)));
    else
      this->layout_field (null_pointer_node);

    /* uint m_align;  */
    this->layout_field (build_integer_cst (ti->alignsize (), d_uint_type));

    /* immutable(void)* xgetRTInfo;  */
    if (sd->getRTInfo)
      this->layout_field (build_expr (sd->getRTInfo, true));
    else if (m_flags & StructFlags::hasPointers)
      this->layout_field (size_one_node);
  }

  /* Layout of TypeInfo_Tuple is:
	void **__vptr;
	void *__monitor;
	TypeInfo[] elements;  */

  void visit (TypeInfoTupleDeclaration *d) final override
  {
    TypeTuple *ti = d->tinfo->isTypeTuple ();

    /* The vtable for TypeInfo_Tuple.  */
    this->layout_base (Type::typeinfotypelist);

    /* TypeInfo[] elements;  */
    Type *satype = Type::tvoidptr->sarrayOf (ti->arguments->length);
    vec<constructor_elt, va_gc> *elms = NULL;
    for (size_t i = 0; i < ti->arguments->length; i++)
      {
	Parameter *arg = (*ti->arguments)[i];
	CONSTRUCTOR_APPEND_ELT (elms, size_int (i),
				build_typeinfo (d->loc, arg->type));
      }
    tree ctor = build_constructor (build_ctype (satype), elms);
    tree decl = this->internal_reference (ctor);

    tree length = size_int (ti->arguments->length);
    tree ptr = build_address (decl);
    this->layout_field (d_array_value (array_type_node, length, ptr));

    rest_of_decl_compilation (decl, 1, 0);
  }
};


/* Main entry point for TypeInfoVisitor interface to generate
   TypeInfo constructor for the TypeInfoDeclaration AST class D.  */

tree
layout_typeinfo (TypeInfoDeclaration *d)
{
  if (!Type::dtypeinfo)
    create_frontend_tinfo_types ();

  TypeInfoVisitor v = TypeInfoVisitor (get_typeinfo_decl (d));
  d->accept (&v);
  return v.result ();
}

/* Like layout_typeinfo, but generates the TypeInfo_Class for
   the class or interface declaration CD.  */

tree
layout_classinfo (ClassDeclaration *cd)
{
  if (!Type::dtypeinfo)
    create_frontend_tinfo_types ();

  TypeInfoClassDeclaration *d = TypeInfoClassDeclaration::create (cd->type);
  TypeInfoVisitor v = TypeInfoVisitor (get_classinfo_decl (cd));
  d->accept (&v);
  return v.result ();
}

/* Get the offset to the BC's vtbl[] initializer from the start of CD.
   Returns "~0u" if the base class is not found in any vtable interfaces.  */

unsigned
base_vtable_offset (ClassDeclaration *cd, BaseClass *bc)
{
  unsigned csymoffset = int_size_in_bytes (tinfo_types[TK_CLASSINFO_TYPE]);
  unsigned interfacesize = int_size_in_bytes (vtbl_interface_type_node);
  csymoffset += cd->vtblInterfaces->length * interfacesize;

  for (size_t i = 0; i < cd->vtblInterfaces->length; i++)
    {
      BaseClass *b = (*cd->vtblInterfaces)[i];
      if (b == bc)
	return csymoffset;
      csymoffset += b->sym->vtbl.length * target.ptrsize;
    }

  /* Check all overriding interface vtbl[]s.  */
  for (ClassDeclaration *cd2 = cd->baseClass; cd2; cd2 = cd2->baseClass)
    {
      for (size_t k = 0; k < cd2->vtblInterfaces->length; k++)
	{
	  BaseClass *bs = (*cd2->vtblInterfaces)[k];
	  if (bs->fillVtbl (cd, NULL, 0))
	    {
	      if (bc == bs)
		return csymoffset;
	      csymoffset += bs->sym->vtbl.length * target.ptrsize;
	    }
	}
    }

  return ~0u;
}

/* Layout fields that immediately come after the classinfo type for DECL if
   there's any interfaces or interface vtables to be added.
   This must be mirrored with base_vtable_offset().  */

static tree
layout_classinfo_interfaces (ClassDeclaration *decl)
{
  tree type = tinfo_types[TK_CLASSINFO_TYPE];
  size_t structsize = int_size_in_bytes (type);

  if (decl->vtblInterfaces->length)
    {
      size_t interfacesize = int_size_in_bytes (vtbl_interface_type_node);
      tree field;

      type = copy_aggregate_type (type);

      /* First layout the static array of Interface, which provides information
	 about the vtables that follow.  */
      tree domain = size_int (decl->vtblInterfaces->length - 1);
      tree arrtype = build_array_type (vtbl_interface_type_node,
				       build_index_type (domain));
      field = create_field_decl (arrtype, NULL, 1, 1);
      insert_aggregate_field (type, field, structsize);
      structsize += decl->vtblInterfaces->length * interfacesize;

      /* For each interface, layout each vtable.  */
      for (size_t i = 0; i < decl->vtblInterfaces->length; i++)
	{
	  BaseClass *b = (*decl->vtblInterfaces)[i];
	  ClassDeclaration *id = b->sym;
	  unsigned offset = base_vtable_offset (decl, b);

	  if (id->vtbl.length && offset != ~0u)
	    {
	      tree vtbldomain
		= build_index_type (size_int (id->vtbl.length - 1));
	      tree vtbltype = build_array_type (vtable_entry_type, vtbldomain);

	      field = create_field_decl (vtbltype, NULL, 1, 1);
	      insert_aggregate_field (type, field, offset);
	      structsize += id->vtbl.length * target.ptrsize;
	    }
	}
    }

  /* Layout the arrays of overriding interface vtables.  */
  for (ClassDeclaration *bcd = decl->baseClass; bcd; bcd = bcd->baseClass)
    {
      for (size_t i = 0; i < bcd->vtblInterfaces->length; i++)
	{
	  BaseClass *b = (*bcd->vtblInterfaces)[i];
	  ClassDeclaration *id = b->sym;
	  unsigned offset = base_vtable_offset (decl, b);

	  if (id->vtbl.length && offset != ~0u)
	    {
	      if (type == tinfo_types[TK_CLASSINFO_TYPE])
		type = copy_aggregate_type (type);

	      tree vtbldomain
		= build_index_type (size_int (id->vtbl.length - 1));
	      tree vtbltype = build_array_type (vtable_entry_type, vtbldomain);

	      tree field = create_field_decl (vtbltype, NULL, 1, 1);
	      insert_aggregate_field (type, field, offset);
	      structsize += id->vtbl.length * target.ptrsize;
	    }
	}
    }

  /* Update the type size and record mode for the classinfo type.  */
  if (type != tinfo_types[TK_CLASSINFO_TYPE])
    finish_aggregate_type (structsize, TYPE_ALIGN_UNIT (type), type);

  return type;
}

/* Returns true if the TypeInfo for TYPE should be placed in
   the runtime library.  */

static bool
builtin_typeinfo_p (Type *type)
{
  if (type->isTypeBasic () || type->ty == TY::Tclass || type->ty == TY::Tnull)
    return !type->mod;

  if (type->ty == TY::Tarray)
    {
      /* Strings are so common, make them builtin.  */
      Type *next = type->nextOf ();
      return !type->mod
	&& ((next->isTypeBasic () != NULL && !next->mod)
	    || (next->ty == TY::Tchar && next->mod == MODimmutable)
	    || (next->ty == TY::Tchar && next->mod == MODconst));
    }

  return false;
}

/* Implements a visitor interface to create the decl tree for TypeInfo decls.
   TypeInfo_Class objects differ in that they also have information about
   the class type packed immediately after the TypeInfo symbol.

   If the frontend had an interface to allow distinguishing being these two
   AST types, then that would be better for us.  */

class TypeInfoDeclVisitor : public Visitor
{
  using Visitor::visit;

public:
  TypeInfoDeclVisitor (void)
  {
  }

  void visit (TypeInfoDeclaration *tid) final override
  {
    tree ident = get_identifier (tid->ident->toChars ());
    tree type = tinfo_types[get_typeinfo_kind (tid->tinfo)];
    gcc_assert (type != NULL_TREE);

    /* Built-in typeinfo will be referenced as one-only.  */
    tid->csym = declare_extern_var (ident, type);
    DECL_LANG_SPECIFIC (tid->csym) = build_lang_decl (tid);

    DECL_CONTEXT (tid->csym) = d_decl_context (tid);
    TREE_READONLY (tid->csym) = 1;
  }

  void visit (TypeInfoClassDeclaration *tid) final override
  {
    TypeClass *tc = tid->tinfo->isTypeClass ();
    tid->csym = get_classinfo_decl (tc->sym);
  }
};

/* Get the VAR_DECL of the TypeInfo for DECL.  If this does not yet exist,
   create it.  The TypeInfo decl provides information about the type of a given
   expression or object.  */

tree
get_typeinfo_decl (TypeInfoDeclaration *decl)
{
  if (decl->csym)
    return decl->csym;

  gcc_assert (decl->tinfo->ty != TY::Terror);

  TypeInfoDeclVisitor v = TypeInfoDeclVisitor ();
  decl->accept (&v);
  gcc_assert (decl->csym != NULL_TREE);

  return decl->csym;
}

/* Get the VAR_DECL of the ClassInfo for DECL.  If this does not yet exist,
   create it.  The ClassInfo decl provides information about the dynamic type
   of a given class type or object.  */

tree
get_classinfo_decl (ClassDeclaration *decl)
{
  if (decl->csym)
    return decl->csym;

  InterfaceDeclaration *id = decl->isInterfaceDeclaration ();
  tree ident = mangle_internal_decl (decl, id ? "__Interface" : "__Class", "Z");
  tree type = layout_classinfo_interfaces (decl);

  decl->csym = declare_extern_var (ident, type);
  DECL_LANG_SPECIFIC (decl->csym) = build_lang_decl (NULL);

  /* Class is a reference, want the record type.  */
  DECL_CONTEXT (decl->csym) = TREE_TYPE (build_ctype (decl->type));
  /* ClassInfo cannot be const data, because we use the monitor on it.  */
  TREE_READONLY (decl->csym) = 0;

  return decl->csym;
}

/* Performs sanity checks on the `object.TypeInfo' type, raising an error if
   RTTI is disabled, or the type is missing.  LOC is the location used for error
   messages.  SC is the context, and EXPR is expression where TypeInfo is
   required from, if either are set.  */

void
check_typeinfo_type (const Loc &loc, Scope *sc, Expression *expr)
{
  if (!global.params.useTypeInfo)
    {
      /* Even when compiling without RTTI we should still be able to evaluate
	 TypeInfo at compile-time, just not at run-time.  */
      if (!sc || !(sc->flags & SCOPEctfe))
	{
	  static int warned = 0;

	  if (expr != NULL)
	    error_at (make_location_t (loc),
		     "expression %qs requires %<object.TypeInfo%> and cannot "
		     "be used with %<-fno-rtti%>", expr->toChars ());
	  else if (!warned)
	    error_at (make_location_t (loc),
		      "%<object.TypeInfo%> cannot be used with %<-fno-rtti%>");

	  warned = 1;
	}
    }

  if (Type::dtypeinfo == NULL
      || (Type::dtypeinfo->storage_class & STCtemp))
    {
      /* If TypeInfo has not been declared, warn about each location once.  */
      static Loc warnloc;

      if (loc.filename && !warnloc.equals (loc))
	{
	  error_at (make_location_t (loc),
		    "%<object.TypeInfo%> could not be found, "
		    "but is implicitly used");
	  warnloc = loc;
	}
    }
}

/* Returns typeinfo reference for TYPE.  LOC is the location used for error
   messages.  EXPR is the expression where TypeInfo is required, if set.  */

tree
build_typeinfo (const Loc &loc, Type *type, Expression *expr)
{
  gcc_assert (type->ty != TY::Terror);
  check_typeinfo_type (loc, NULL, expr);
  create_typeinfo (type, NULL);
  return build_address (get_typeinfo_decl (type->vtinfo));
}

tree build_typeinfo (Expression *expr, Type *type)
{
  return build_typeinfo (expr->loc, type, expr);
}

/* Like layout_classinfo, but generates an Object that wraps around a
   pointer to C++ type_info so it can be distinguished from D TypeInfo.  */

void
layout_cpp_typeinfo (ClassDeclaration *cd)
{
  if (!Type::dtypeinfo)
    create_frontend_tinfo_types ();

  gcc_assert (cd->isCPPclass ());

  tree decl = get_cpp_typeinfo_decl (cd);
  vec<constructor_elt, va_gc> *init = NULL;

  /* Use the vtable of __cpp_type_info_ptr, the EH personality routine
     expects this, as it uses .classinfo identity comparison to test for
     C++ catch handlers.  */
  ClassDeclaration *cppti = ClassDeclaration::cpp_type_info_ptr;
  if (have_typeinfo_p (cppti))
    {
      tree vptr = get_vtable_decl (cppti);
      CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, build_address (vptr));
    }
  else
    CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, null_pointer_node);

  if (cppti->hasMonitor ())
    CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, null_pointer_node);

  /* Let C++ do the RTTI generation, and just reference the symbol as
     extern, knowing the underlying type is not required.  */
  const char *ident = target.cpp.typeInfoMangle (cd);
  tree typeinfo = declare_extern_var (get_identifier (ident),
				      unknown_type_node);
  TREE_READONLY (typeinfo) = 1;
  CONSTRUCTOR_APPEND_ELT (init, NULL_TREE, build_address (typeinfo));

  /* Build the initializer and emit.  */
  DECL_INITIAL (decl) = build_struct_literal (TREE_TYPE (decl), init);
  d_finish_decl (decl);
}

/* Get the VAR_DECL of the __cpp_type_info_ptr for DECL.  If this does not yet
   exist, create it.  The __cpp_type_info_ptr decl is then initialized with a
   pointer to the C++ type_info for the given class.  */

tree
get_cpp_typeinfo_decl (ClassDeclaration *decl)
{
  gcc_assert (decl->isCPPclass ());

  if (decl->cpp_type_info_ptr_sym)
    return decl->cpp_type_info_ptr_sym;

  if (!tinfo_types[TK_CPPTI_TYPE])
    make_internal_typeinfo (TK_CPPTI_TYPE,
			    Identifier::idPool ("__cpp_type_info_ptr"),
			    ptr_type_node, NULL);

  tree ident = mangle_internal_decl (decl, "_cpp_type_info_ptr", "");
  tree type = tinfo_types[TK_CPPTI_TYPE];

  decl->cpp_type_info_ptr_sym = declare_extern_var (ident, type);
  DECL_LANG_SPECIFIC (decl->cpp_type_info_ptr_sym) = build_lang_decl (NULL);

  /* Class is a reference, want the record type.  */
  DECL_CONTEXT (decl->cpp_type_info_ptr_sym)
    = TREE_TYPE (build_ctype (decl->type));
  TREE_READONLY (decl->cpp_type_info_ptr_sym) = 1;

  /* Layout the initializer and emit the symbol.  */
  layout_cpp_typeinfo (decl);

  return decl->cpp_type_info_ptr_sym;
}

/* Get the exact TypeInfo for TYPE, if it doesn't exist, create it.  */

void
create_typeinfo (Type *type, Module *mod)
{
  if (!Type::dtypeinfo)
    create_frontend_tinfo_types ();

  /* Do this since not all Type's are merged.  */
  Type *t = type->merge2 ();
  Identifier *ident;

  if (!t->vtinfo)
    {
      tinfo_kind tk = get_typeinfo_kind (t);
      switch (tk)
	{
	case TK_SHARED_TYPE:
	case TK_CONST_TYPE:
	case TK_IMMUTABLE_TYPE:
	case TK_INOUT_TYPE:
	case TK_POINTER_TYPE:
	case TK_ARRAY_TYPE:
	case TK_VECTOR_TYPE:
	case TK_INTERFACE_TYPE:
	  /* Kinds of TypeInfo that add one extra pointer field.  */
	  if (tk == TK_SHARED_TYPE)
	    {
	      /* Does both `shared' and `shared const'.  */
	      t->vtinfo = TypeInfoSharedDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Shared");
	    }
	  else if (tk == TK_CONST_TYPE)
	    {
	      t->vtinfo = TypeInfoConstDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Const");
	    }
	  else if (tk == TK_IMMUTABLE_TYPE)
	    {
	      t->vtinfo = TypeInfoInvariantDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Invariant");
	    }
	  else if (tk == TK_INOUT_TYPE)
	    {
	      t->vtinfo = TypeInfoWildDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Wild");
	    }
	  else if (tk == TK_POINTER_TYPE)
	    {
	      t->vtinfo = TypeInfoPointerDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Pointer");
	    }
	  else if (tk == TK_ARRAY_TYPE)
	    {
	      t->vtinfo = TypeInfoArrayDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Array");
	    }
	  else if (tk == TK_VECTOR_TYPE)
	    {
	      t->vtinfo = TypeInfoVectorDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Vector");
	    }
	  else if (tk == TK_INTERFACE_TYPE)
	    {
	      t->vtinfo = TypeInfoInterfaceDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Interface");
	    }
	  else
	    gcc_unreachable ();

	  if (!tinfo_types[tk])
	    make_internal_typeinfo (tk, ident, ptr_type_node, NULL);
	  break;

	case TK_STATICARRAY_TYPE:
	  if (!tinfo_types[tk])
	    {
	      ident = Identifier::idPool ("TypeInfo_StaticArray");
	      make_internal_typeinfo (tk, ident, ptr_type_node, size_type_node,
				      NULL);
	    }
	  t->vtinfo = TypeInfoStaticArrayDeclaration::create (t);
	  break;

	case TK_ASSOCIATIVEARRAY_TYPE:
	  if (!tinfo_types[tk])
	    {
	      ident = Identifier::idPool ("TypeInfo_AssociativeArray");
	      make_internal_typeinfo (tk, ident, ptr_type_node, ptr_type_node,
				      NULL);
	    }
	  t->vtinfo = TypeInfoAssociativeArrayDeclaration::create (t);
	  break;

	case TK_STRUCT_TYPE:
	  if (!tinfo_types[tk])
	    {
	      ident = Identifier::idPool ("TypeInfo_Struct");
	      make_internal_typeinfo (tk, ident,
				      array_type_node, array_type_node,
				      ptr_type_node, ptr_type_node,
				      ptr_type_node, ptr_type_node,
				      d_uint_type, ptr_type_node,
				      ptr_type_node, d_uint_type,
				      ptr_type_node, NULL);
	    }
	  t->vtinfo = TypeInfoStructDeclaration::create (t);
	  break;

	case TK_ENUMERAL_TYPE:
	  if (!tinfo_types[tk])
	    {
	      ident = Identifier::idPool ("TypeInfo_Enum");
	      make_internal_typeinfo (tk, ident,
				      ptr_type_node, array_type_node,
				      array_type_node, NULL);
	    }
	  t->vtinfo = TypeInfoEnumDeclaration::create (t);
	  break;

	case TK_FUNCTION_TYPE:
	case TK_DELEGATE_TYPE:
	  /* Functions and delegates share a common TypeInfo layout.  */
	  if (tk == TK_FUNCTION_TYPE)
	    {
	      t->vtinfo = TypeInfoFunctionDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Function");
	    }
	  else if (tk == TK_DELEGATE_TYPE)
	    {
	      t->vtinfo = TypeInfoDelegateDeclaration::create (t);
	      ident = Identifier::idPool ("TypeInfo_Delegate");
	    }
	  else
	    gcc_unreachable ();

	  if (!tinfo_types[tk])
	    make_internal_typeinfo (tk, ident, ptr_type_node,
				    array_type_node, NULL);
	  break;

	case TK_TYPELIST_TYPE:
	  if (!tinfo_types[tk])
	    {
	      ident = Identifier::idPool ("TypeInfo_Tuple");
	      make_internal_typeinfo (tk, ident, array_type_node, NULL);
	    }
	  t->vtinfo = TypeInfoTupleDeclaration::create (t);
	  break;

	case TK_CLASSINFO_TYPE:
	  t->vtinfo = TypeInfoClassDeclaration::create (t);
	  break;

	default:
	  t->vtinfo = TypeInfoDeclaration::create (t);
	}
      gcc_assert (t->vtinfo);

      /* If this has a custom implementation in rt/typeinfo, then
	 do not generate a COMDAT for it.  */
      if (!builtin_typeinfo_p (t))
	{
	  /* Find module that will go all the way to an object file.  */
	  if (mod)
	    mod->members->push (t->vtinfo);
	  else
	    build_decl_tree (t->vtinfo);
	}
    }
  /* Types aren't merged, but we can share the vtinfo's.  */
  if (!type->vtinfo)
    type->vtinfo = t->vtinfo;

  gcc_assert (type->vtinfo != NULL);
}

/* Implements a visitor interface to check whether a type is speculative.
   TypeInfo_Struct would reference the members of the struct it is representing
   (e.g: opEquals via xopEquals field), so if it's instantiated in speculative
   context, TypeInfo creation should also be stopped to avoid possible
   `unresolved symbol' linker errors.  */

class SpeculativeTypeVisitor : public Visitor
{
  using Visitor::visit;

  bool result_;

public:
  SpeculativeTypeVisitor (void)
  {
    this->result_ = false;
  }

  bool result (void)
  {
    return this->result_;
  }

  void visit (Type *t) final override
  {
    Type *tb = t->toBasetype ();
    if (tb != t)
      tb->accept (this);
  }

  void visit (TypeNext *t) final override
  {
    if (t->next)
      t->next->accept (this);
  }

  void visit (TypeBasic *) final override
  {
  }

  void visit (TypeVector *t) final override
  {
    t->basetype->accept (this);
  }

  void visit (TypeAArray *t) final override
  {
    t->index->accept (this);
    visit ((TypeNext *) t);
  }

  void visit (TypeFunction *t) final override
  {
    visit ((TypeNext *) t);
  }

  void visit (TypeStruct *t) final override
  {
    StructDeclaration *sd = t->sym;
    if (TemplateInstance *ti = sd->isInstantiated ())
      {
	if (!ti->needsCodegen ())
	  {
	    if (ti->minst || sd->requestTypeInfo ())
	      return;

	    this->result_ |= true;
	  }
      }
  }

  void visit (TypeClass *t) final override
  {
    ClassDeclaration *cd = t->sym;
    if (TemplateInstance *ti = cd->isInstantiated ())
      {
	if (!ti->needsCodegen () && !ti->minst)
	  {
	    this->result_ |= true;
	  }
      }
  }

  void visit (TypeTuple *t) final override
  {
    if (!t->arguments)
      return;

    for (size_t i = 0; i < t->arguments->length; i++)
      {
	Type *tprm = (*t->arguments)[i]->type;
	if (tprm)
	  tprm->accept (this);
	if (this->result_)
	  return;
      }
  }
};

/* Return true if type was instantiated in a speculative context.  */

bool
speculative_type_p (Type *t)
{
  SpeculativeTypeVisitor v = SpeculativeTypeVisitor ();
  t->accept (&v);
  return v.result ();
}

#include "gt-d-typeinfo.h"
