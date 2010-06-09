/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             A D A - T R E E                              *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2010, Free Software Foundation, Inc.         *
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
	   chain_next ("(union lang_tree_node *)TREE_CHAIN (&%h.generic)")))
  lang_tree_node
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
};

/* Ada uses the lang_decl and lang_type fields to hold a tree.

   FIXME: the variable_size annotation here is needed because these types are
   variable-sized in some other front-ends.  Due to gengtype deficiency, the
   GTY options of such types have to agree across all front-ends.  */
struct GTY((variable_size)) lang_type { tree t; };
struct GTY((variable_size)) lang_decl { tree t; };

/* Macros to get and set the tree in TYPE_LANG_SPECIFIC.  */
#define GET_TYPE_LANG_SPECIFIC(NODE) \
  (TYPE_LANG_SPECIFIC (NODE) ? TYPE_LANG_SPECIFIC (NODE)->t : NULL_TREE)

#define SET_TYPE_LANG_SPECIFIC(NODE, X)			 \
do {							 \
  tree tmp = (X);					 \
  if (!TYPE_LANG_SPECIFIC (NODE))			 \
    TYPE_LANG_SPECIFIC (NODE)				 \
      = ggc_alloc_lang_type (sizeof (struct lang_type)); \
  TYPE_LANG_SPECIFIC (NODE)->t = tmp;			 \
} while (0)

/* Macros to get and set the tree in DECL_LANG_SPECIFIC.  */
#define GET_DECL_LANG_SPECIFIC(NODE) \
  (DECL_LANG_SPECIFIC (NODE) ? DECL_LANG_SPECIFIC (NODE)->t : NULL_TREE)

#define SET_DECL_LANG_SPECIFIC(NODE, X)			 \
do {							 \
  tree tmp = (X);					 \
  if (!DECL_LANG_SPECIFIC (NODE))			 \
    DECL_LANG_SPECIFIC (NODE)				 \
      = ggc_alloc_lang_decl (sizeof (struct lang_decl)); \
  DECL_LANG_SPECIFIC (NODE)->t = tmp;			 \
} while (0)


/* Flags added to type nodes.  */

/* For RECORD_TYPE, UNION_TYPE, and QUAL_UNION_TYPE, nonzero if this is a
   record being used as a fat pointer (only true for RECORD_TYPE).  */
#define TYPE_FAT_POINTER_P(NODE) \
  TYPE_LANG_FLAG_0 (RECORD_OR_UNION_CHECK (NODE))

#define TYPE_IS_FAT_POINTER_P(NODE) \
  (TREE_CODE (NODE) == RECORD_TYPE && TYPE_FAT_POINTER_P (NODE))

/* For integral types and array types, nonzero if this is a packed array type
   used for bit-packed types.  Such types should not be extended to a larger
   size or validated against a specified size.  */
#define TYPE_PACKED_ARRAY_TYPE_P(NODE) TYPE_LANG_FLAG_0 (NODE)

#define TYPE_IS_PACKED_ARRAY_TYPE_P(NODE) \
  ((TREE_CODE (NODE) == INTEGER_TYPE || TREE_CODE (NODE) == ARRAY_TYPE) \
   && TYPE_PACKED_ARRAY_TYPE_P (NODE))

/* For INTEGER_TYPE, nonzero if this is a modular type with a modulus that
   is not equal to two to the power of its mode's size.  */
#define TYPE_MODULAR_P(NODE) TYPE_LANG_FLAG_1 (INTEGER_TYPE_CHECK (NODE))

/* For ARRAY_TYPE, nonzero if this type corresponds to a dimension of
   an Ada array other than the first.  */
#define TYPE_MULTI_ARRAY_P(NODE) TYPE_LANG_FLAG_1 (ARRAY_TYPE_CHECK (NODE))

/* For FUNCTION_TYPE, nonzero if this denotes a function returning an
   unconstrained array or record.  */
#define TYPE_RETURN_UNCONSTRAINED_P(NODE) \
  TYPE_LANG_FLAG_1 (FUNCTION_TYPE_CHECK (NODE))

/* For RECORD_TYPE, UNION_TYPE, and QUAL_UNION_TYPE, nonzero if this denotes
   a justified modular type (will only be true for RECORD_TYPE).  */
#define TYPE_JUSTIFIED_MODULAR_P(NODE) \
  TYPE_LANG_FLAG_1 (RECORD_OR_UNION_CHECK (NODE))

/* Nonzero in an arithmetic subtype if this is a subtype not known to the
   front-end.  */
#define TYPE_EXTRA_SUBTYPE_P(NODE) TYPE_LANG_FLAG_2 (NODE)

/* For RECORD_TYPE, UNION_TYPE, and QUAL_UNION_TYPE, nonzero if this is the
   type for an object whose type includes its template in addition to
   its value (only true for RECORD_TYPE).  */
#define TYPE_CONTAINS_TEMPLATE_P(NODE) \
  TYPE_LANG_FLAG_3 (RECORD_OR_UNION_CHECK (NODE))

/* For INTEGER_TYPE, nonzero if this really represents a VAX
   floating-point type.  */
#define TYPE_VAX_FLOATING_POINT_P(NODE) \
  TYPE_LANG_FLAG_3 (INTEGER_TYPE_CHECK (NODE))

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

/* For FUNCTION_TYPEs, nonzero if the function returns by direct reference,
   i.e. the callee returns a pointer to a memory location it has allocated
   and the caller only needs to dereference the pointer.  */
#define TYPE_RETURN_BY_DIRECT_REF_P(NODE) \
  TYPE_LANG_FLAG_4 (FUNCTION_TYPE_CHECK (NODE))

/* For VOID_TYPE, ENUMERAL_TYPE, UNION_TYPE, and RECORD_TYPE, nonzero if this
   is a dummy type, made to correspond to a private or incomplete type.  */
#define TYPE_DUMMY_P(NODE) TYPE_LANG_FLAG_4 (NODE)

#define TYPE_IS_DUMMY_P(NODE) \
  ((TREE_CODE (NODE) == VOID_TYPE || TREE_CODE (NODE) == RECORD_TYPE	\
    || TREE_CODE (NODE) == UNION_TYPE || TREE_CODE (NODE) == ENUMERAL_TYPE) \
   && TYPE_DUMMY_P (NODE))

/* For an INTEGER_TYPE, nonzero if TYPE_ACTUAL_BOUNDS is present.  */
#define TYPE_HAS_ACTUAL_BOUNDS_P(NODE) \
  TYPE_LANG_FLAG_5 (INTEGER_TYPE_CHECK (NODE))

/* For a RECORD_TYPE, nonzero if this was made just to supply needed
   padding or alignment.  */
#define TYPE_PADDING_P(NODE) TYPE_LANG_FLAG_5 (RECORD_TYPE_CHECK (NODE))

#define TYPE_IS_PADDING_P(NODE) \
  (TREE_CODE (NODE) == RECORD_TYPE && TYPE_PADDING_P (NODE))

/* True if TYPE can alias any other types.  */
#define TYPE_UNIVERSAL_ALIASING_P(NODE) TYPE_LANG_FLAG_6 (NODE)

/* In an UNCONSTRAINED_ARRAY_TYPE, this is the record containing both the
   template and the object.

   ??? We also put this on an ENUMERAL_TYPE that is dummy.  Technically,
   this is a conflict on the minval field, but there doesn't seem to be
   simple fix, so we'll live with this kludge for now.  */
#define TYPE_OBJECT_RECORD_TYPE(NODE) \
  (TREE_CHECK2 ((NODE), UNCONSTRAINED_ARRAY_TYPE, ENUMERAL_TYPE)->type.minval)

/* For numerical types, this is the GCC lower bound of the type.  The GCC
   type system is based on the invariant that an object X of a given type
   cannot hold at run time a value smaller than its lower bound; otherwise
   the behavior is undefined.  The optimizer takes advantage of this and
   considers that the assertion X >= LB is always true.  */
#define TYPE_GCC_MIN_VALUE(NODE) (NUMERICAL_TYPE_CHECK (NODE)->type.minval)

/* For numerical types, this is the GCC upper bound of the type.  The GCC
   type system is based on the invariant that an object X of a given type
   cannot hold at run time a value larger than its upper bound; otherwise
   the behavior is undefined.  The optimizer takes advantage of this and
   considers that the assertion X <= UB is always true.  */
#define TYPE_GCC_MAX_VALUE(NODE) (NUMERICAL_TYPE_CHECK (NODE)->type.maxval)

/* For a FUNCTION_TYPE, if the subprogram has parameters passed by copy in/
   copy out, this is the list of nodes used to specify the return values of
   the out (or in out) parameters that are passed by copy in/copy out.  For
   a full description of the copy in/copy out parameter passing mechanism
   refer to the routine gnat_to_gnu_entity.  */
#define TYPE_CI_CO_LIST(NODE) TYPE_LANG_SLOT_1 (FUNCTION_TYPE_CHECK (NODE))

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
   sense, hence the GCC lower bound be set to that of the base type.  */
#define TYPE_RM_MIN_VALUE(NODE) TYPE_RM_VALUE ((NODE), 1)
#define SET_TYPE_RM_MIN_VALUE(NODE, X) SET_TYPE_RM_VALUE ((NODE), 1, (X))

/* For numerical types, this is the RM upper bound of the type.  There is
   again a discrepancy between this upper bound and the GCC upper bound,
   again because of the need to support invalid values.

   These values can be outside the range of values allowed by the RM upper
   bound but they must nevertheless be valid in the GCC type system, otherwise
   the optimizer can pretend that they simply don't exist.  Therefore they
   must be within the range of values allowed by the upper bound in the GCC
   sense, hence the GCC upper bound be set to that of the base type.  */
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
#define TYPE_MODULUS(NODE) GET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE))
#define SET_TYPE_MODULUS(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE), X)

/* For an INTEGER_TYPE with TYPE_VAX_FLOATING_POINT_P, this is the
   Digits_Value.  */
#define TYPE_DIGITS_VALUE(NODE) \
  GET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE))
#define SET_TYPE_DIGITS_VALUE(NODE, X) \
  SET_TYPE_LANG_SPECIFIC (INTEGER_TYPE_CHECK (NODE), X)

/* For an INTEGER_TYPE that is the TYPE_DOMAIN of some ARRAY_TYPE, this is
   the type corresponding to the Ada index type.  */
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

/* For a RECORD_TYPE that is a fat pointer, this is the type for the
   unconstrained object.  Likewise for a RECORD_TYPE that is pointed
   to by a thin pointer.  */
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


/* Flags added to decl nodes.  */

/* Nonzero in a FUNCTION_DECL that represents a stubbed function
   discriminant.  */
#define DECL_STUBBED_P(NODE) DECL_LANG_FLAG_0 (FUNCTION_DECL_CHECK (NODE))

/* Nonzero in a VAR_DECL if it is guaranteed to be constant after having
   been elaborated and TREE_READONLY is not set on it.  */
#define DECL_READONLY_ONCE_ELAB(NODE) DECL_LANG_FLAG_0 (VAR_DECL_CHECK (NODE))

/* Nonzero in a CONST_DECL if its value is (essentially) the address of a
   constant CONSTRUCTOR.  */
#define DECL_CONST_ADDRESS_P(NODE) DECL_LANG_FLAG_0 (CONST_DECL_CHECK (NODE))

/* Nonzero if this decl is always used by reference; i.e., an INDIRECT_REF
   is needed to access the object.  */
#define DECL_BY_REF_P(NODE) DECL_LANG_FLAG_1 (NODE)

/* Nonzero in a FIELD_DECL that is a dummy built for some internal reason.  */
#define DECL_INTERNAL_P(NODE) DECL_LANG_FLAG_3 (FIELD_DECL_CHECK (NODE))

/* Nonzero if this decl is a PARM_DECL for an Ada array being passed to a
   foreign convention subprogram.  */
#define DECL_BY_COMPONENT_PTR_P(NODE) DECL_LANG_FLAG_3 (PARM_DECL_CHECK (NODE))

/* Nonzero in a FUNCTION_DECL that corresponds to an elaboration procedure.  */
#define DECL_ELABORATION_PROC_P(NODE) \
  DECL_LANG_FLAG_3 (FUNCTION_DECL_CHECK (NODE))

/* Nonzero if this is a decl for a pointer that points to something which
   is readonly.  Used mostly for fat pointers.  */
#define DECL_POINTS_TO_READONLY_P(NODE) DECL_LANG_FLAG_4 (NODE)

/* Nonzero in a PARM_DECL if we are to pass by descriptor.  */
#define DECL_BY_DESCRIPTOR_P(NODE) DECL_LANG_FLAG_5 (PARM_DECL_CHECK (NODE))

/* Nonzero in a VAR_DECL if it is a pointer renaming a global object.  */
#define DECL_RENAMING_GLOBAL_P(NODE) DECL_LANG_FLAG_5 (VAR_DECL_CHECK (NODE))

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

/* In a VAR_DECL, points to the object being renamed if the VAR_DECL is a
   renaming pointer, otherwise 0.  Note that this object is guaranteed to
   be protected against multiple evaluations.  */
#define DECL_RENAMED_OBJECT(NODE) \
  GET_DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))
#define SET_DECL_RENAMED_OBJECT(NODE, X) \
  SET_DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE), X)

/* In a TYPE_DECL, points to the parallel type if any, otherwise 0.  */
#define DECL_PARALLEL_TYPE(NODE) \
  GET_DECL_LANG_SPECIFIC (TYPE_DECL_CHECK (NODE))
#define SET_DECL_PARALLEL_TYPE(NODE, X) \
  SET_DECL_LANG_SPECIFIC (TYPE_DECL_CHECK (NODE), X)

/* In a FUNCTION_DECL, points to the stub associated with the function
   if any, otherwise 0.  */
#define DECL_FUNCTION_STUB(NODE) \
  GET_DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (NODE))
#define SET_DECL_FUNCTION_STUB(NODE, X) \
  SET_DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (NODE), X)

/* In a PARM_DECL, points to the alternate TREE_TYPE.  */
#define DECL_PARM_ALT_TYPE(NODE) \
  GET_DECL_LANG_SPECIFIC (PARM_DECL_CHECK (NODE))
#define SET_DECL_PARM_ALT_TYPE(NODE, X) \
  SET_DECL_LANG_SPECIFIC (PARM_DECL_CHECK (NODE), X)


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

#define EXIT_STMT_COND(NODE)     TREE_OPERAND_CHECK_CODE (NODE, EXIT_STMT, 0)
#define EXIT_STMT_LABEL(NODE)    TREE_OPERAND_CHECK_CODE (NODE, EXIT_STMT, 1)
