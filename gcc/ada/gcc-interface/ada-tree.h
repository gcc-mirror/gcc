/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             A D A - T R E E                              *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2025, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have received a copy of the GNU General   *
 * Public License along with GCC; see the file COPYING3.  If not see        *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* The resulting tree type.  */
union GTY((desc ("0"),
	   chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
  lang_tree_node
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
};

/* Ada uses the lang_decl and lang_type fields to hold a tree.  */
struct GTY(()) lang_type { tree t1; tree t2; };
struct GTY(()) lang_decl { tree t; };

extern struct lang_type *get_lang_specific (tree node);

/* Macros to get and set the trees in TYPE_LANG_SPECIFIC.  */
#define GET_TYPE_LANG_SPECIFIC(NODE) \
  (TYPE_LANG_SPECIFIC (NODE) ? TYPE_LANG_SPECIFIC (NODE)->t1 : NULL_TREE)

#define SET_TYPE_LANG_SPECIFIC(NODE, X) (get_lang_specific (NODE)->t1 = (X))

#define GET_TYPE_LANG_SPECIFIC2(NODE) \
  (TYPE_LANG_SPECIFIC (NODE) ? TYPE_LANG_SPECIFIC (NODE)->t2 : NULL_TREE)

#define SET_TYPE_LANG_SPECIFIC2(NODE, X) (get_lang_specific (NODE)->t2 = (X))

/* Macros to get and set the tree in DECL_LANG_SPECIFIC.  */
#define GET_DECL_LANG_SPECIFIC(NODE) \
  (DECL_LANG_SPECIFIC (NODE) ? DECL_LANG_SPECIFIC (NODE)->t : NULL_TREE)

#define SET_DECL_LANG_SPECIFIC(NODE, X)			 \
do {							 \
  tree tmp = (X);					 \
  if (!DECL_LANG_SPECIFIC (NODE))			 \
    DECL_LANG_SPECIFIC (NODE)				 \
      = ggc_alloc<struct lang_decl> (); \
  DECL_LANG_SPECIFIC (NODE)->t = tmp;			 \
} while (0)


/* Flags added to type nodes.  */

/* For RECORD_TYPE, UNION_TYPE, and QUAL_UNION_TYPE, nonzero if this is a
   record being used as a fat pointer (only true for RECORD_TYPE).  */
#define TYPE_FAT_POINTER_P(NODE) \
  TYPE_LANG_FLAG_0 (RECORD_OR_UNION_CHECK (NODE))

#define TYPE_IS_FAT_POINTER_P(NODE) \
  (TREE_CODE (NODE) == RECORD_TYPE && TYPE_FAT_POINTER_P (NODE))

/* For integral types and array types, nonzero if this is an implementation
   type for a bit-packed array type.  Such types should not be extended to a
   larger size or validated against a specified size.  */
#define TYPE_BIT_PACKED_ARRAY_TYPE_P(NODE) \
  TYPE_LANG_FLAG_0 (TREE_CHECK2 (NODE, INTEGER_TYPE, ARRAY_TYPE))

#define BIT_PACKED_ARRAY_TYPE_P(NODE) \
  ((TREE_CODE (NODE) == INTEGER_TYPE || TREE_CODE (NODE) == ARRAY_TYPE) \
   && TYPE_BIT_PACKED_ARRAY_TYPE_P (NODE))

/* For FUNCTION_TYPE and METHOD_TYPE, nonzero if the function returns by
   direct reference, i.e. the callee returns a pointer to a memory location
   it has allocated and the caller only needs to dereference the pointer.  */
#define TYPE_RETURN_BY_DIRECT_REF_P(NODE) \
  TYPE_LANG_FLAG_0 (FUNC_OR_METHOD_CHECK (NODE))

/* For INTEGER_TYPE, nonzero if this is a modular type with a modulus that
   is not equal to two to the power of its mode's size.  */
#define TYPE_MODULAR_P(NODE) TYPE_LANG_FLAG_1 (INTEGER_TYPE_CHECK (NODE))

/* For ARRAY_TYPE, nonzero if this type corresponds to a dimension of
   an Ada array other than the first.  */
#define TYPE_MULTI_ARRAY_P(NODE) TYPE_LANG_FLAG_1 (ARRAY_TYPE_CHECK (NODE))

/* For RECORD_TYPE, UNION_TYPE, and QUAL_UNION_TYPE, nonzero if this denotes
   a justified modular type (will only be true for RECORD_TYPE).  */
#define TYPE_JUSTIFIED_MODULAR_P(NODE) \
  TYPE_LANG_FLAG_1 (RECORD_OR_UNION_CHECK (NODE))

/* Nonzero in an arithmetic subtype if this is a subtype not known to the
   front-end.  */
#define TYPE_EXTRA_SUBTYPE_P(NODE) TYPE_LANG_FLAG_2 (INTEGER_TYPE_CHECK (NODE))

#define TYPE_IS_EXTRA_SUBTYPE_P(NODE) \
  (TREE_CODE (NODE) == INTEGER_TYPE && TYPE_EXTRA_SUBTYPE_P (NODE))

/* Nonzero for an aggregate type if this is a by-reference type.  We also
   set this on an ENUMERAL_TYPE that is dummy.  */
#define TYPE_BY_REFERENCE_P(NODE)				       \
  TYPE_LANG_FLAG_2 (TREE_CHECK5 (NODE, RECORD_TYPE, UNION_TYPE,	       \
				 ARRAY_TYPE, UNCONSTRAINED_ARRAY_TYPE, \
				 ENUMERAL_TYPE))

#define TYPE_IS_BY_REFERENCE_P(NODE)		    \
  ((TREE_CODE (NODE) == RECORD_TYPE		    \
    || TREE_CODE (NODE) == UNION_TYPE		    \
    || TREE_CODE (NODE) == ARRAY_TYPE		    \
    || TREE_CODE (NODE) == UNCONSTRAINED_ARRAY_TYPE \
    || TREE_CODE (NODE) == ENUMERAL_TYPE)	    \
   && TYPE_BY_REFERENCE_P (NODE))

/* For RECORD_TYPE, UNION_TYPE, and QUAL_UNION_TYPE, nonzero if this is the
   type for an object whose type includes its template in addition to
   its value (only true for RECORD_TYPE).  */
#define TYPE_CONTAINS_TEMPLATE_P(NODE) \
  TYPE_LANG_FLAG_3 (RECORD_OR_UNION_CHECK (NODE))

/* For INTEGER_TYPE, nonzero if it implements a fixed-point type.  */
#define TYPE_FIXED_POINT_P(NODE) \
  TYPE_LANG_FLAG_3 (INTEGER_TYPE_CHECK (NODE))

#define TYPE_IS_FIXED_POINT_P(NODE) \
  (TREE_CODE (NODE) == INTEGER_TYPE && TYPE_FIXED_POINT_P (NODE))

/* True if NODE is a thin pointer.  */
#define TYPE_IS_THIN_POINTER_P(NODE)			\
  (POINTER_TYPE_P (NODE)				\
   && TREE_CODE (TREE_TYPE (NODE)) == RECORD_TYPE	\
   && TYPE_CONTAINS_TEMPLATE_P (TREE_TYPE (NODE)))

/* True if TYPE is either a fat or thin pointer to an unconstrained
   array.  */
#define TYPE_IS_FAT_OR_THIN_POINTER_P(NODE) \
  (TYPE_IS_FAT_POINTER_P (NODE) || TYPE_IS_THIN_POINTER_P (NODE))

/* For INTEGER_TYPEs, nonzero if the type has a biased representation.  */
#define TYPE_BIASED_REPRESENTATION_P(NODE) \
  TYPE_LANG_FLAG_4 (INTEGER_TYPE_CHECK (NODE))

/* For ARRAY_TYPEs, nonzero if the array type has Convention_Fortran.  */
#define TYPE_CONVENTION_FORTRAN_P(NODE) \
  TYPE_LANG_FLAG_4 (ARRAY_TYPE_CHECK (NODE))

/* For RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE, nonzero if this is a dummy
   type, made to correspond to a private or incomplete type.  */
#define TYPE_DUMMY_P(NODE) \
  TYPE_LANG_FLAG_4 (TREE_CHECK3 (NODE, RECORD_TYPE, UNION_TYPE, ENUMERAL_TYPE))

#define TYPE_IS_DUMMY_P(NODE)		  \
  ((TREE_CODE (NODE) == RECORD_TYPE	  \
    || TREE_CODE (NODE) == UNION_TYPE	  \
    || TREE_CODE (NODE) == ENUMERAL_TYPE) \
   && TYPE_DUMMY_P (NODE))

/* For an INTEGER_TYPE, nonzero if TYPE_ACTUAL_BOUNDS is present.  */
#define TYPE_HAS_ACTUAL_BOUNDS_P(NODE) \
  TYPE_LANG_FLAG_5 (INTEGER_TYPE_CHECK (NODE))

/* For a RECORD_TYPE, nonzero if this was made just to supply needed
   padding or alignment.  */
#define TYPE_PADDING_P(NODE) TYPE_LANG_FLAG_5 (RECORD_TYPE_CHECK (NODE))

#define TYPE_IS_PADDING_P(NODE) \
  (TREE_CODE (NODE) == RECORD_TYPE && TYPE_PADDING_P (NODE))

/* True for a non-dummy type if TYPE can alias any other types.  */
#define TYPE_UNIVERSAL_ALIASING_P(NODE) TYPE_LANG_FLAG_6 (NODE)

/* True for a dummy type if TYPE appears in a profile.  */
#define TYPE_DUMMY_IN_PROFILE_P(NODE) TYPE_LANG_FLAG_6 (NODE)

/* True if objects of this type are guaranteed to be properly aligned.  */
#define TYPE_ALIGN_OK(NODE) TYPE_LANG_FLAG_7 (NODE)

/* True for types that implement a packed array and for original packed array
   types.  */
#define TYPE_IMPL_PACKED_ARRAY_P(NODE) \
  ((TREE_CODE (NODE) == ARRAY_TYPE && TYPE_PACKED (NODE)) \
   || (TREE_CODE (NODE) == INTEGER_TYPE && TYPE_BIT_PACKED_ARRAY_TYPE_P (NODE)))

/* True for types that can hold a debug type.  */
#define TYPE_CAN_HAVE_DEBUG_TYPE_P(NODE) (!TYPE_IMPL_PACKED_ARRAY_P (NODE))

/* For RECORD_TYPE, UNION_TYPE, and QUAL_UNION_TYPE, this holds the maximum
   alignment value the type ought to have.  */
#define TYPE_MAX_ALIGN(NODE) (TYPE_PRECISION (RECORD_OR_UNION_CHECK (NODE)))

/* For an UNCONSTRAINED_ARRAY_TYPE, this is the record containing both the
   template and the object.

   ??? We also put this on an ENUMERAL_TYPE that is dummy.  Technically,
   this is a conflict on the minval field, but there doesn't seem to be
   simple fix, so we'll live with this kludge for now.  */
#define TYPE_OBJECT_RECORD_TYPE(NODE) \
  (TYPE_MIN_VALUE_RAW (TREE_CHECK2 ((NODE), UNCONSTRAINED_ARRAY_TYPE, \
				    ENUMERAL_TYPE)))

/* For numerical types, this is the GCC lower bound of the type.  The GCC
   type system is based on the invariant that an object X of a given type
   cannot hold at run time a value smaller than its lower bound; otherwise
   the behavior is undefined.  The optimizer takes advantage of this and
   considers that the assertion X >= LB is always true.  */
#define TYPE_GCC_MIN_VALUE(NODE) \
  (TYPE_MIN_VALUE_RAW (NUMERICAL_TYPE_CHECK (NODE)))

/* For numerical types, this is the GCC upper bound of the type.  The GCC
   type system is based on the invariant that an object X of a given type
   cannot hold at run time a value larger than its upper bound; otherwise
   the behavior is undefined.  The optimizer takes advantage of this and
   considers that the assertion X <= UB is always true.  */
#define TYPE_GCC_MAX_VALUE(NODE) \
  (TYPE_MAX_VALUE_RAW (NUMERICAL_TYPE_CHECK (NODE)))

/* For a FUNCTION_TYPE and METHOD_TYPE, if the function has parameters passed
   by copy in/copy out, this is the list of nodes used to specify the return
   values of these parameters.  For a full description of the copy in/copy out
   parameter passing mechanism refer to the routine gnat_to_gnu_entity.  */
#define TYPE_CI_CO_LIST(NODE) TYPE_LANG_SLOT_1 (FUNC_OR_METHOD_CHECK (NODE))

/* For an ARRAY_TYPE with variable size, this is the padding type built for
   the array type when it is itself the component type of another array.  */
#define TYPE_PADDING_FOR_COMPONENT(NODE) \
  TYPE_LANG_SLOT_1 (ARRAY_TYPE_CHECK (NODE))

/* For a VECTOR_TYPE, this is the representative array type.  */
#define TYPE_REPRESENTATIVE_ARRAY(NODE) \
  TYPE_LANG_SLOT_1 (VECTOR_TYPE_CHECK (NODE))

/* For numerical types, this holds various RM-defined values.  */
#define TYPE_RM_VALUES(NODE) TYPE_LANG_SLOT_1 (NUMERICAL_TYPE_CHECK (NODE))

/* Macros to get and set the individual values in TYPE_RM_VALUES.  */
#define TYPE_RM_VALUE(NODE, N)				    \
  (TYPE_RM_VALUES (NODE)				    \
   ? TREE_VEC_ELT (TYPE_RM_VALUES (NODE), (N)) : NULL_TREE)

#define SET_TYPE_RM_VALUE(NODE, N, X)		   \
do {						   \
  tree tmp = (X);				   \
  if (!TYPE_RM_VALUES (NODE))			   \
    TYPE_RM_VALUES (NODE) = make_tree_vec (3);	   \
  /* ??? The field is not visited by the generic   \
     code so we need to mark it manually.  */	   \
  MARK_VISITED (tmp);				   \
  TREE_VEC_ELT (TYPE_RM_VALUES (NODE), (N)) = tmp; \
} while (0)

/* For numerical types, this is the RM size of the type, aka its precision.
   There is a discrepancy between what is called precision here (and more
   generally throughout gigi) and what is called precision in the GCC type
   system: in the former case it's TYPE_RM_SIZE whereas it's TYPE_PRECISION
   in the latter case.  They are not identical because of the need to support
   invalid values.

   These values can be outside the range of values allowed by the RM size
   but they must nevertheless be valid in the GCC type system, otherwise
   the optimizer can pretend that they simply don't exist.  Therefore they
   must be within the range of values allowed by the precision in the GCC
   sense, hence TYPE_PRECISION be set to the Esize, not the RM size.  */
#define TYPE_RM_SIZE(NODE) TYPE_RM_VALUE ((NODE), 0)
#define SET_TYPE_RM_SIZE(NODE, X) SET_TYPE_RM_VALUE ((NODE), 0, (X))

/* For numerical types, this is the RM lower bound of the type.  There is
   again a discrepancy between this lower bound and the GCC lower bound,
   again because of the need to support invalid values.

   These values can be outside the range of values allowed by the RM lower
   bound but they must nevertheless be valid in the GCC type system, otherwise
   the optimizer can pretend that they simply don't exist.  Therefore they
   must be within the range of values allowed by the lower bound in the GCC
   sense, hence the GCC lower bound be set to that of the base type.

   This lower bound is translated directly without the adjustments that may
   be required for type compatibility, so it will generally be necessary to
   convert it to the base type of the numerical type before using it.  */
#define TYPE_RM_MIN_VALUE(NODE) TYPE_RM_VALUE ((NODE), 1)
#define SET_TYPE_RM_MIN_VALUE(NODE, X) SET_TYPE_RM_VALUE ((NODE), 1, (X))

/* For numerical types, this is the RM upper bound of the type.  There is
   again a discrepancy between this upper bound and the GCC upper bound,
   again because of the need to support invalid values.

   These values can be outside the range of values allowed by the RM upper
   bound but they must nevertheless be valid in the GCC type system, otherwise
   the optimizer can pretend that they simply don't exist.  Therefore they
   must be within the range of values allowed by the upper bound in the GCC
   sense, hence the GCC upper bound be set to that of the base type.

   This upper bound is translated directly without the adjustments that may
   be required for type compatibility, so it will generally be necessary to
   convert it to the base type of the numerical type before using it.  */
#define TYPE_RM_MAX_VALUE(NODE) TYPE_RM_VALUE ((NODE), 2)
#define SET_TYPE_RM_MAX_VALUE(NODE, X) SET_TYPE_RM_VALUE ((NODE), 2, (X))

/* For numerical types, this is the lower bound of the type, i.e. the RM lower
   bound for language-defined types and the GCC lower bound for others.  */
#undef TYPE_MIN_VALUE
#define TYPE_MIN_VALUE(NODE) \
  (TYPE_RM_MIN_VALUE (NODE) \
   ? TYPE_RM_MIN_VALUE (NODE) : TYPE_GCC_MIN_VALUE (NODE))

/* For numerical types, this is the upper bound of the type, i.e. the RM upper
   bound for language-defined types and the GCC upper bound for others.  */
#undef TYPE_MAX_VALUE
#define TYPE_MAX_VALUE(NODE) \
  (TYPE_RM_MAX_VALUE (NODE) \
   ? TYPE_RM_MAX_VALUE (NODE) : TYPE_GCC_MAX_VALUE (NODE))

/* For an INTEGER_TYPE with TYPE_MODULAR_P, this is the value of the
   modulus. */
#define TYPE_MODULUS(NODE) \
  GET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE))
#define SET_TYPE_MODULUS(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE), X)

/* For an INTEGER_TYPE that is the TYPE_DOMAIN of some ARRAY_TYPE, this is
   the type corresponding to the Ada index type.  It is necessary to keep
   these 2 views for every array type because the TYPE_DOMAIN is subject
   to strong constraints in GENERIC: it must be a subtype of SIZETYPE and
   may not be superflat, i.e. the upper bound must always be larger or
   equal to the lower bound minus 1 (i.e. the canonical length formula
   must always yield a non-negative number), which means that at least
   one of the bounds may need to be a conditional expression.  There are
   no such constraints on the TYPE_INDEX_TYPE because gigi is prepared to
   deal with the superflat case; moreover the TYPE_INDEX_TYPE is used as
   the index type for the debug info and, therefore, needs to be as close
   as possible to the source index type.  */
#define TYPE_INDEX_TYPE(NODE) \
  GET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE))
#define SET_TYPE_INDEX_TYPE(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE), X)

/* For an INTEGER_TYPE with TYPE_HAS_ACTUAL_BOUNDS_P or an ARRAY_TYPE, this is
   the index type that should be used when the actual bounds are required for
   a template.  This is used in the case of packed arrays.  */
#define TYPE_ACTUAL_BOUNDS(NODE) \
  GET_TYPE_LANG_SPECIFIC (TREE_CHECK2 (NODE, INTEGER_TYPE, ARRAY_TYPE))
#define SET_TYPE_ACTUAL_BOUNDS(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (TREE_CHECK2 (NODE, INTEGER_TYPE, ARRAY_TYPE), X)

/* For a POINTER_TYPE that points to the template type of an unconstrained
   array type, this is the address to be used in a null fat pointer.  */
#define TYPE_NULL_BOUNDS(NODE) \
  GET_TYPE_LANG_SPECIFIC (POINTER_TYPE_CHECK (NODE))
#define SET_TYPE_NULL_BOUNDS(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (POINTER_TYPE_CHECK (NODE), X)

/* For a RECORD_TYPE that is a fat pointer, this is the type for the
   unconstrained array.  Likewise for a RECORD_TYPE that is pointed
   to by a thin pointer, if it is made for the unconstrained array
   type itself; the field is NULL_TREE if the RECORD_TYPE is made
   for a constrained subtype of the array type.  */
#define TYPE_UNCONSTRAINED_ARRAY(NODE) \
  GET_TYPE_LANG_SPECIFIC (RECORD_TYPE_CHECK (NODE))
#define SET_TYPE_UNCONSTRAINED_ARRAY(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (RECORD_TYPE_CHECK (NODE), X)

/* For other RECORD_TYPEs and all UNION_TYPEs and QUAL_UNION_TYPEs, this is
   the Ada size of the object.  This differs from the GCC size in that it
   does not include any rounding up to the alignment of the type.  */
#define TYPE_ADA_SIZE(NODE) \
  GET_TYPE_LANG_SPECIFIC (RECORD_OR_UNION_CHECK (NODE))
#define SET_TYPE_ADA_SIZE(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (RECORD_OR_UNION_CHECK (NODE), X)

/* For an INTEGER_TYPE with TYPE_IS_FIXED_POINT_P, this is the value of the
   scale factor.  Modular types, index types (sizetype subtypes) and
   fixed-point types are totally distinct types, so there is no problem with
   sharing type lang specific's first slot.  */
#define TYPE_SCALE_FACTOR(NODE) \
  GET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE))
#define SET_TYPE_SCALE_FACTOR(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE), X)

/* For types with TYPE_CAN_HAVE_DEBUG_TYPE_P, this is the type to use in
   debugging information.  */
#define TYPE_DEBUG_TYPE(NODE) \
  GET_TYPE_LANG_SPECIFIC2 (NODE)
#define SET_TYPE_DEBUG_TYPE(NODE, X) \
  SET_TYPE_LANG_SPECIFIC2 (NODE, X)

/* For types with TYPE_IMPL_PACKED_ARRAY_P, this is the original packed
   array type.  Note that this predicate is true for original packed array
   types, so these cannot have a debug type.  */
#define TYPE_ORIGINAL_PACKED_ARRAY(NODE) \
  GET_TYPE_LANG_SPECIFIC2 (NODE)
#define SET_TYPE_ORIGINAL_PACKED_ARRAY(NODE, X) \
  SET_TYPE_LANG_SPECIFIC2 (NODE, X)


/* Flags added to decl nodes.  */

/* Nonzero in a VAR_DECL if it is guaranteed to be constant after having
   been elaborated and TREE_READONLY is not set on it.  */
#define DECL_READONLY_ONCE_ELAB(NODE) DECL_LANG_FLAG_0 (VAR_DECL_CHECK (NODE))

/* Nonzero in a CONST_DECL if its value is (essentially) the address of a
   constant CONSTRUCTOR.  */
#define DECL_CONST_ADDRESS_P(NODE) DECL_LANG_FLAG_0 (CONST_DECL_CHECK (NODE))

/* Nonzero in a FIELD_DECL if it is declared as aliased.  */
#define DECL_ALIASED_P(NODE) DECL_LANG_FLAG_0 (FIELD_DECL_CHECK (NODE))

/* Nonzero in a TYPE_DECL if this is the declaration of a Taft amendment type
   in the main unit, i.e. the full declaration is available.  */
#define DECL_TAFT_TYPE_P(NODE) DECL_LANG_FLAG_0 (TYPE_DECL_CHECK (NODE))

/* Nonzero in a PARM_DECL passed by reference but for which only a restricted
   form of aliasing is allowed.  The first restriction comes explicitly from
   the RM 6.2(12) clause: there is no read-after-write dependency between a
   store based on such a PARM_DECL and a load not based on this PARM_DECL,
   so stores based on such PARM_DECLs can be sunk past all loads based on
   a distinct object.  The second restriction can be inferred from the same
   clause: there is no write-after-write dependency between a store based
   on such a PARM_DECL and a store based on a distinct such PARM_DECL, as
   the compiler would be allowed to pass the parameters by copy and the
   order of assignment to actual parameters after a call is arbitrary as
   per the RM 6.4.1(17) clause, so stores based on distinct such PARM_DECLs
   can be swapped.  */
#define DECL_RESTRICTED_ALIASING_P(NODE) \
  DECL_LANG_FLAG_0 (PARM_DECL_CHECK (NODE))

/* Nonzero in a DECL if it is always used by reference, i.e. an INDIRECT_REF
   is needed to access the object.  */
#define DECL_BY_REF_P(NODE) DECL_LANG_FLAG_1 (NODE)

/* Nonzero in a DECL if it is made for a pointer that can never be null.  */
#define DECL_CAN_NEVER_BE_NULL_P(NODE) DECL_LANG_FLAG_2 (NODE)

/* Nonzero in a VAR_DECL if it is made for a loop parameter.  */
#define DECL_LOOP_PARM_P(NODE) DECL_LANG_FLAG_3 (VAR_DECL_CHECK (NODE))

/* Nonzero in a FIELD_DECL that is a dummy built for some internal reason.  */
#define DECL_INTERNAL_P(NODE) DECL_LANG_FLAG_3 (FIELD_DECL_CHECK (NODE))

/* Nonzero in a PARM_DECL if it is made for an Ada array being passed to a
   foreign convention subprogram.  */
#define DECL_BY_COMPONENT_PTR_P(NODE) DECL_LANG_FLAG_3 (PARM_DECL_CHECK (NODE))

/* Nonzero in a FUNCTION_DECL that corresponds to an elaboration procedure.  */
#define DECL_ELABORATION_PROC_P(NODE) \
  DECL_LANG_FLAG_3 (FUNCTION_DECL_CHECK (NODE))

/* Nonzero in a CONST_DECL, VAR_DECL or PARM_DECL if it is made for a pointer
   that points to something which is readonly.  */
#define DECL_POINTS_TO_READONLY_P(NODE) DECL_LANG_FLAG_4 (NODE)

/* Nonzero in a FIELD_DECL if it is invariant once set, for example if it is
   a discriminant of a discriminated type without default expression.  */
#define DECL_INVARIANT_P(NODE) DECL_LANG_FLAG_4 (FIELD_DECL_CHECK (NODE))

/* Nonzero in a FUNCTION_DECL if this is a definition, i.e. if it was created
   by a call to gnat_to_gnu_entity with definition set to True.  */
#define DECL_FUNCTION_IS_DEF(NODE) \
  DECL_LANG_FLAG_4 (FUNCTION_DECL_CHECK (NODE))

/* Nonzero in a VAR_DECL if it is a temporary created to hold the return
   value of a function call or 'reference to a function call.  */
#define DECL_RETURN_VALUE_P(NODE) DECL_LANG_FLAG_5 (VAR_DECL_CHECK (NODE))

/* Nonzero in a PARM_DECL if its mechanism was forced to by-reference.  */
#define DECL_FORCED_BY_REF_P(NODE) DECL_LANG_FLAG_5 (PARM_DECL_CHECK (NODE))

/* In a FIELD_DECL corresponding to a discriminant, contains the
   discriminant number.  */
#define DECL_DISCRIMINANT_NUMBER(NODE) DECL_INITIAL (FIELD_DECL_CHECK (NODE))

/* In a CONST_DECL, points to a VAR_DECL that is allocatable to
   memory.  Used when a scalar constant is aliased or has its
   address taken.  */
#define DECL_CONST_CORRESPONDING_VAR(NODE) \
  GET_DECL_LANG_SPECIFIC (CONST_DECL_CHECK (NODE))
#define SET_DECL_CONST_CORRESPONDING_VAR(NODE, X) \
  SET_DECL_LANG_SPECIFIC (CONST_DECL_CHECK (NODE), X)

/* In a FIELD_DECL, points to the FIELD_DECL that was the ultimate
   source of the decl.  */
#define DECL_ORIGINAL_FIELD(NODE) \
  GET_DECL_LANG_SPECIFIC (FIELD_DECL_CHECK (NODE))
#define SET_DECL_ORIGINAL_FIELD(NODE, X) \
  SET_DECL_LANG_SPECIFIC (FIELD_DECL_CHECK (NODE), X)

/* Set DECL_ORIGINAL_FIELD of FIELD1 to (that of) FIELD2.  */
#define SET_DECL_ORIGINAL_FIELD_TO_FIELD(FIELD1, FIELD2)	\
  SET_DECL_ORIGINAL_FIELD ((FIELD1),				\
			   DECL_ORIGINAL_FIELD (FIELD2)		\
			   ? DECL_ORIGINAL_FIELD (FIELD2) : (FIELD2))

/* Return true if FIELD1 and FIELD2 represent the same field.  */
#define SAME_FIELD_P(FIELD1, FIELD2)					\
  ((FIELD1) == (FIELD2)							\
   || DECL_ORIGINAL_FIELD (FIELD1) == (FIELD2)				\
   || (FIELD1) == DECL_ORIGINAL_FIELD (FIELD2)				\
   || (DECL_ORIGINAL_FIELD (FIELD1)					\
       && (DECL_ORIGINAL_FIELD (FIELD1) == DECL_ORIGINAL_FIELD (FIELD2))))

/* In a VAR_DECL with the DECL_LOOP_PARM_P flag set, points to the special
   induction variable that is built under certain circumstances, if any.  */
#define DECL_INDUCTION_VAR(NODE) \
  GET_DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))
#define SET_DECL_INDUCTION_VAR(NODE, X) \
  SET_DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE), X)

/* In a TYPE_DECL, points to the parallel type if any, otherwise 0.  */
#define DECL_PARALLEL_TYPE(NODE) \
  GET_DECL_LANG_SPECIFIC (TYPE_DECL_CHECK (NODE))
#define SET_DECL_PARALLEL_TYPE(NODE, X) \
  SET_DECL_LANG_SPECIFIC (TYPE_DECL_CHECK (NODE), X)


/* Flags added to ref nodes.  */

/* Nonzero means this node will not trap.  */
#undef TREE_THIS_NOTRAP
#define TREE_THIS_NOTRAP(NODE) \
  (TREE_CHECK4 (NODE, INDIRECT_REF, ARRAY_REF, UNCONSTRAINED_ARRAY_REF, \
		ARRAY_RANGE_REF)->base.nothrow_flag)


/* Fields and macros for statements.  */
#define IS_ADA_STMT(NODE) \
  (STATEMENT_CLASS_P (NODE) && TREE_CODE (NODE) >= STMT_STMT)

#define STMT_STMT_STMT(NODE)     TREE_OPERAND_CHECK_CODE (NODE, STMT_STMT, 0)

#define LOOP_STMT_COND(NODE)     TREE_OPERAND_CHECK_CODE (NODE, LOOP_STMT, 0)
#define LOOP_STMT_UPDATE(NODE)   TREE_OPERAND_CHECK_CODE (NODE, LOOP_STMT, 1)
#define LOOP_STMT_BODY(NODE)     TREE_OPERAND_CHECK_CODE (NODE, LOOP_STMT, 2)
#define LOOP_STMT_LABEL(NODE)    TREE_OPERAND_CHECK_CODE (NODE, LOOP_STMT, 3)

/* A loop statement is conceptually made up of 6 sub-statements:

    loop:
      TOP_CONDITION
      TOP_UPDATE
      BODY
      BOTTOM_CONDITION
      BOTTOM_UPDATE
      GOTO loop

  However, only 4 of them can exist for a given loop, the pair of conditions
  and the pair of updates being mutually exclusive.  The default setting is
  TOP_CONDITION and BOTTOM_UPDATE and the following couple of flags are used
  to toggle the individual settings.  */
#define LOOP_STMT_BOTTOM_COND_P(NODE) TREE_LANG_FLAG_0 (LOOP_STMT_CHECK (NODE))
#define LOOP_STMT_TOP_UPDATE_P(NODE)  TREE_LANG_FLAG_1 (LOOP_STMT_CHECK (NODE))

/* Optimization hints on loops.  */
#define LOOP_STMT_IVDEP(NODE)     TREE_LANG_FLAG_2 (LOOP_STMT_CHECK (NODE))
#define LOOP_STMT_NO_UNROLL(NODE) TREE_LANG_FLAG_3 (LOOP_STMT_CHECK (NODE))
#define LOOP_STMT_UNROLL(NODE)    TREE_LANG_FLAG_4 (LOOP_STMT_CHECK (NODE))
#define LOOP_STMT_NO_VECTOR(NODE) TREE_LANG_FLAG_5 (LOOP_STMT_CHECK (NODE))
#define LOOP_STMT_VECTOR(NODE)    TREE_LANG_FLAG_6 (LOOP_STMT_CHECK (NODE))

#define EXIT_STMT_COND(NODE)     TREE_OPERAND_CHECK_CODE (NODE, EXIT_STMT, 0)
#define EXIT_STMT_LABEL(NODE)    TREE_OPERAND_CHECK_CODE (NODE, EXIT_STMT, 1)

/* Small kludge to be able to define Ada built-in functions locally.
   We overload them on top of the C++ coroutines builtin functions.  */
#define BUILT_IN_LIKELY      BUILT_IN_CORO_PROMISE
#define BUILT_IN_UNLIKELY    BUILT_IN_CORO_RESUME
#define BUILT_IN_RETURN_SLOT BUILT_IN_CORO_DESTROY
