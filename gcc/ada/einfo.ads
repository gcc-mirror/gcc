------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E I N F O                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off); -- with/use clauses for children
with Namet;  use Namet;
with Snames; use Snames;
with Stand;  use Stand;
with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;
pragma Warnings (On);

package Einfo is

--  This package documents the annotations to the abstract syntax tree that are
--  needed to support semantic processing of an Ada compilation.

--  See the spec of Gen_IL.Gen for instructions on making changes to this file.
--  Note that the official definition of what entities have what fields is in
--  Gen_IL.Gen.Gen_Entities; if there is a discrepancy between that and the
--  comments here, Gen_IL.Gen.Gen_Entities wins.

--  These annotations are for the most part attributes of declared entities,
--  and they correspond to conventional symbol table information. Other
--  attributes include sets of meanings for overloaded names, possible
--  types for overloaded expressions, flags to indicate deferred constants,
--  incomplete types, etc. These attributes are stored in fields in
--  tree nodes.

--  There are two kinds of semantic information

--    First, the tree nodes with the following Nkind values:

--      N_Defining_Identifier
--      N_Defining_Character_Literal
--      N_Defining_Operator_Symbol

--    are called Entities, and constitute the information that would often
--    be stored separately in a symbol table. These nodes are all extended
--    to provide extra space, and contain fields which depend on the entity
--    kind, as defined by the contents of the Ekind field. The use of the
--    Ekind field, and the associated fields in the entity, are defined
--    in this package, as are the access functions to these fields.

--    Second, in some cases semantic information is stored directly in other
--    kinds of nodes, e.g. the Etype field, used to indicate the type of an
--    expression. These fields are defined in the Sinfo package, but their
--    full documentation is in the Einfo package specification.

--  Declaration processing places information in the nodes of their defining
--  identifiers. Name resolution places in all other occurrences of an
--  identifier a pointer to the corresponding defining occurrence.

----------------------------------
-- Handling of Type'Size Values --
----------------------------------

--  The Ada 95 RM contains some rather peculiar (to us) rules on the value
--  of type'Size (see RM 13.3(55)). We have found that attempting to use
--  these RM Size values generally, and in particular for determining the
--  default size of objects, creates chaos, and major incompatibilities in
--  existing code.

--  The Ada 2022 RM acknowledges it and adopts GNAT's Object_Size attribute
--  for determining the default size of objects, but stops short of applying
--  it universally like GNAT. Indeed the notable exceptions are nonaliased
--  stand-alone objects, which are not covered by Object_Size in Ada 2022.

--  We proceed as follows, for discrete and fixed-point subtypes, we have
--  two separate sizes for each subtype:

--    The Object_Size, which is used for determining the default size of
--    objects and components. This size value can be referred to using the
--    Object_Size attribute. The phrase "is used" here means that it is
--    the basis of the determination of the size. The back end is free to
--    pad this up if necessary for efficiency, e.g. an 8-bit stand-alone
--    character might be stored in 32 bits on a machine with no efficient
--    byte access instructions such as the Alpha.

--    The default rules for the value of Object_Size are as follows:

--       The Object_Size for base subtypes reflect the natural hardware
--       size in bits (see Ttypes and Cstand for integer types). For
--       enumeration and fixed-point base subtypes have 8, 16, 32, or 64
--       bits for this size, depending on the range of values to be stored.

--       The Object_Size of a subtype is the same as the Object_Size of
--       the subtype from which it is obtained.

--       The Object_Size of a derived base type is copied from the parent
--       base type, and the Object_Size of a derived first subtype is copied
--       from the parent first subtype.

--    The Ada 2022 RM defined attribute Object_Size uses this implementation.

--    The Value_Size, which is the number of bits required to store a value
--    of the type. This size can be referred to using the Value_Size
--    attribute. This value is used for determining how tightly to pack
--    records or arrays with components of this type, and also affects
--    the semantics of unchecked conversion (unchecked conversions where
--    the Value_Size values differ generate a warning, and are potentially
--    target dependent).

--    The default rules for the value of Value_Size are as follows:

--       The Value_Size for a base subtype is the minimum number of bits
--       required to store all values of the type (including the sign bit
--       only if negative values are possible).

--       If a subtype statically matches the first subtype, then it has
--       by default the same Value_Size as the first subtype. This is a
--       consequence of RM 13.1(14) ("if two subtypes statically match,
--       then their subtype-specific aspects are the same".)

--       All other subtypes have a Value_Size corresponding to the minimum
--       number of bits required to store all values of the subtype. For
--       dynamic bounds, it is assumed that the value can range down or up
--       to the corresponding bound of the ancestor.

--    The Ada 95 RM defined attribute Size is identified with Value_Size.

--    The Size attribute may be defined for a first-named subtype. This sets
--    the Value_Size of the first-named subtype to the given value, and the
--    Object_Size of this first-named subtype to the given value padded up
--    to an appropriate boundary. It is a consequence of the default rules
--    above that this Object_Size will apply to all further subtypes. On the
--    other hand, Value_Size is affected only for the first subtype, any
--    dynamic subtypes obtained from it directly, and any statically matching
--    subtypes. The Value_Size of any other static subtypes is not affected.

--    Value_Size and Object_Size may be explicitly set for any subtype using
--    an attribute definition clause. Note that the use of such a clause can
--    cause the RM 13.1(14) rule to be violated, in Ada 95 and 2022 for the
--    Value_Size attribute, but only in Ada 95 for the Object_Size attribute.
--    If access types reference aliased objects whose subtypes have differing
--    Object_Size values as a result of explicit attribute definition clauses,
--    then it is erroneous to convert from one access subtype to the other.

--    At the implementation level, the Esize field stores the Object_Size
--    and the RM_Size field stores the Value_Size (hence the value of the
--    Size attribute, which, as noted above, is equivalent to Value_Size).

--  To get a feel for the difference, consider the following examples (note
--  that in each case the base is short_short_integer with a size of 8):

--                                            Object_Size     Value_Size

--     type x1 is range 0..5;                      8               3

--     type x2 is range 0..5;
--     for x2'size use 12;                        16              12

--     subtype x3 is x2 range 0 .. 3;             16               2

--     subtype x4 is x2'base range 0 .. 10;        8               4

--     dynamic : x2'Base range -64 .. +63;

--     subtype x5 is x2 range 0 .. dynamic;       16               3*

--     subtype x6 is x2'base range 0 .. dynamic;   8               7*

--  Note: the entries marked * are not actually specified by the Ada 95 RM,
--  but it seems in the spirit of the RM rules to allocate the minimum number
--  of bits known to be large enough to hold the given range of values.

--  So far, so good, but GNAT has to obey the RM rules, so the question is
--  under what conditions must the RM Size be used. The following is a list
--  of the occasions on which the RM Size must be used:

--    Component size for packed arrays or records
--    Value of the attribute Size for a type
--    Warning about sizes not matching for unchecked conversion

--  The RM_Size field keeps track of the RM Size as needed in these
--  three situations.

--  For elementary types other than discrete and fixed-point types, the
--  Object_Size and Value_Size are the same (and equivalent to the RM
--  attribute Size). Only Size may be specified for such types.

--  For composite types, Object_Size and Value_Size are computed from their
--  respective value for the type of each element as well as the layout.

--  All size attributes are stored as Uint values. Negative values are used to
--  reference GCC expressions for the case of non-static sizes, as explained
--  in Repinfo.

--------------------------------------
-- Delayed Freezing and Elaboration --
--------------------------------------

--  The flag Has_Delayed_Freeze indicates that an entity carries an explicit
--  freeze node, which appears later in the expanded tree.

--  a) The flag is used by the front end to trigger expansion activities which
--  include the generation of that freeze node. Typically this happens at the
--  end of the current compilation unit, or before the first subprogram body is
--  encountered in the current unit. See units Freeze and Exp_Ch13 for details
--  on the actions triggered by a freeze node, which include the construction
--  of initialization procedures and dispatch tables.

--  b) The flag is used by the back end to defer elaboration of the entity
--  until its freeze node is seen. In the absence of an explicit freeze node,
--  an entity is frozen (and elaborated) at the point of declaration.

--  For object declarations, the flag is set when an address clause for the
--  object is encountered. Legality checks on the address expression only take
--  place at the freeze point of the object. In Ada 2012, the flag is also set
--  when an address aspect for the object is encountered.

--  Most types have an explicit freeze node, because they cannot be elaborated
--  until all representation and operational items that apply to them have been
--  analyzed. Private types and incomplete types have the flag set as well, as
--  do task and protected types.

--  Implicit base types created for type derivations, as well as class-wide
--  types created for all tagged types, have the flag set.

--  If a subprogram has an access parameter whose designated type is incomplete
--  the subprogram has the flag set.

-----------------------
-- Entity Attributes --
-----------------------

--  This section contains a complete list of the attributes that are defined
--  on entities. Some attributes apply to all entities, others only to certain
--  kinds of entities. In the latter case the attribute should only be set or
--  accessed if the Ekind field indicates an appropriate entity.

--  There are two kinds of attributes that apply to entities, stored and
--  synthesized. Stored attributes correspond to a field or flag in the entity
--  itself. Such attributes are identified in the table below by giving the
--  field or flag in the attribute that is used to hold the attribute value.
--  Synthesized attributes are not stored directly, but are rather computed as
--  needed from other attributes, or from information in the tree. These are
--  marked "synthesized" in the table below. The stored attributes have both
--  access functions and set procedures to set the corresponding values, while
--  synthesized attributes have only access functions.

--  Note: in the case of Node, Uint, or Elist fields, there are cases where the
--  same physical field is used for different purposes in different entities,
--  so these access functions should only be referenced for the class of
--  entities in which they are defined as being present. Flags are not
--  overlapped in this way, but nevertheless as a matter of style and
--  abstraction (which may or may not be checked by assertions in the
--  body), this restriction should be observed for flag fields as well.

--  Note: certain of the attributes on types apply only to base types, and
--  are so noted by the notation [base type only]. These are cases where the
--  attribute of any subtype is the same as the attribute of the base type.
--  The attribute can be referenced on a subtype (and automatically retrieves
--  the value from the base type). However, it is an error to try to set the
--  attribute on other than the base type, and if assertions are enabled,
--  an attempt to set the attribute on a subtype will raise an assert error.

--  Other attributes are noted as applying to the [implementation base type
--  only]. These are representation attributes which must always apply to a
--  full non-private type, and where the attributes are always on the full
--  type. The attribute can be referenced on a subtype (and automatically
--  retrieves the value from the implementation base type). However, it is an
--  error to try to set the attribute on other than the implementation base
--  type.

--  Other attributes are noted as applying to the [root type only]. The
--  attribute can be referenced on a subtype (and automatically retrieves the
--  value from the root type). However, it is an error to try to set the
--  attribute on other than the root type.

--  The definitive definition of what is [... type only] is in Gen_Entities.
--  See calls to Sm passing Base_Type_Only, Impl_Base_Type_Only, or
--  Root_Type_Only.

--    Abstract_States
--       Defined for E_Package entities. Contains a list of all the abstract
--       states declared by the related package.

--    Accept_Address
--       Defined in entries. If an accept has a statement sequence, then an
--       address variable is created, which is used to hold the address of the
--       parameters, as passed by the runtime. Accept_Address holds an element
--       list which represents a stack of entities for these address variables.
--       The current entry is the top of the stack, which is the last element
--       on the list. A stack is required to handle the case of nested select
--       statements referencing the same entry.

--    Access_Disp_Table [implementation base type only]
--       Defined in E_Record_Type and E_Record_Subtype entities. Set in tagged
--       types to point to their dispatch tables. The first two entities are
--       associated with the primary dispatch table: 1) primary dispatch table
--       with user-defined primitives 2) primary dispatch table with predefined
--       primitives. For each interface type covered by the tagged type we also
--       have: 3) secondary dispatch table with thunks of primitives covering
--       user-defined interface primitives, 4) secondary dispatch table with
--       thunks of predefined primitives, 5) secondary dispatch table with user
--       defined primitives, and 6) secondary dispatch table with predefined
--       primitives. The last entity of this list is an access type declaration
--       used to expand dispatching calls through the primary dispatch table.
--       For an untagged record, contains No_Elist.

--    Access_Disp_Table_Elab_Flag [implementation base type only]
--       Defined in E_Record_Type and E_Record_Subtype entities. Set in tagged
--       types whose dispatch table elaboration must be completed at run time
--       by the IP routine to point to its pending elaboration flag entity.
--       This flag is needed when the elaboration of the dispatch table relies
--       on attribute 'Position applied to an object of the type; it is used by
--       the IP routine to avoid performing this elaboration twice.

--    Access_Subprogram_Wrapper
--       Entity created for access_to_subprogram types that have pre/post
--       conditions. Wrapper subprogram is created when analyzing corresponding
--       aspect, and inherits said aspects. Body of subprogram includes code
--       to check contracts, and a direct call to the designated subprogram.
--       The body is part of the freeze actions for the type.
--       The Subprogram_Type created for the Access_To_Subprogram carries the
--       Access_Subprogram_Wrapper for use in the expansion of indirect calls.

--    Activation_Record_Component
--       Defined for E_Variable, E_Constant, E_Loop_Parameter, and formal
--       parameter entities. Used in Opt.Unnest_Subprogram_Mode, in which case
--       a reference to an uplevel entity produces a corresponding component
--       in the generated ARECnT activation record (Exp_Unst for details).

--    Actual_Subtype
--       Defined in variables, constants, and formal parameters. This is the
--       subtype imposed by the value of the object, as opposed to its nominal
--       subtype, which is imposed by the declaration. The actual subtype
--       differs from the nominal one when the latter is indefinite (as in the
--       case of an unconstrained formal parameter, or a variable declared
--       with an unconstrained type and an initial value). The nominal subtype
--       is the Etype entry for the entity. The Actual_Subtype field is set
--       only if the actual subtype differs from the nominal subtype. If the
--       actual and nominal subtypes are the same, then the Actual_Subtype
--       field is Empty, and Etype indicates both types.
--
--       For objects, the Actual_Subtype is set only if this is a discriminated
--       type. For arrays, the bounds of the expression are obtained and the
--       Etype of the object is directly the constrained subtype, except in the
--       case of a return object that lives on the secondary stack where Etype
--       is the nominal unconstrained subtype. This is rather irregular and the
--       semantic checks that depend on the nominal subtype being unconstrained
--       use flag Is_Constr_Subt_For_U_Nominal(qv).

--    Address_Clause (synthesized)
--       Applies to entries, objects and subprograms. Set if an address clause
--       is present which references the object or subprogram and points to
--       the N_Attribute_Definition_Clause node. Empty if no Address clause.
--       The expression in the address clause is always a constant that is
--       defined before the entity to which the address clause applies.
--       Note: The backend references this field in E_Task_Type entities???

--    Address_Taken
--       Defined in all entities. Set if the Address or Unrestricted_Access
--       attribute is applied directly to the entity, i.e. the entity is the
--       entity of the prefix of the attribute reference. Also set if the
--       entity is the second argument of an Asm_Input or Asm_Output attribute,
--       as the construct may entail taking its address. And also set if the
--       entity is a subprogram and the Access or Unchecked_Access attribute is
--       applied. Used by the backend to make sure that the address can be
--       meaningfully taken, and also in the case of subprograms to control
--       output of certain warnings.

--    Aft_Value (synthesized)
--       Applies to fixed-point types and subtypes. This yields the value of
--       the Aft attribute for the type, i.e. the number of decimal digits
--       needed after the decimal point to accommodate the delta of the type,
--       unless the delta is greater than 0.1, in which case it is 1.

--    Alias
--       Defined in overloadable entities (literals, subprograms, entries) and
--       subprograms that cover a primitive operation of an abstract interface
--       (that is, subprograms with the Interface_Alias attribute). In case of
--       overloaded entities it points to the parent subprogram of a derived
--       subprogram. In case of abstract interface subprograms it points to the
--       subprogram that covers the abstract interface primitive. Also used for
--       a subprogram renaming, where it points to the renamed subprogram. For
--       an inherited operation (of a type extension) that is overridden in a
--       private part, the Alias is the overriding operation. In this fashion a
--       call from outside the package ends up executing the new body even if
--       non-dispatching, and a call from inside calls the overriding operation
--       because it hides the implicit one. Alias is always empty for entries.

--    Alignment
--       Defined in entities for types and also in constants, variables
--       (including exceptions where it refers to the static data allocated for
--       an exception), loop parameters, and formal parameters. This indicates
--       the desired alignment for a type, or the actual alignment for an
--       object. A value of zero (Uint_0) indicates that the alignment has not
--       been set yet. The alignment can be set by an explicit alignment
--       clause, or set by the front-end in package Layout, or set by the
--       back-end as part of the back-end back-annotation process. The
--       alignment field is also defined in E_Exception entities, but there it
--       is used only by the back-end for back annotation.

--    Alignment_Clause (synthesized)
--       Applies to all entities for types and objects. If an alignment
--       attribute definition clause is present for the entity, then this
--       function returns the N_Attribute_Definition clause that specifies the
--       alignment. If no alignment clause applies to the type, then the call
--       to this function returns Empty. Note that the call can return a
--       non-Empty value even if Has_Alignment_Clause is not set (happens with
--       subtype and derived type declarations). Note also that a record
--       definition clause with an (obsolescent) mod clause is converted
--       into an attribute definition clause for this purpose.

--    Anonymous_Designated_Type
--       Defined in variables which represent anonymous finalization masters.
--       Contains the designated type which is being serviced by the master.

--    Anonymous_Masters
--       Defined in packages, subprograms, and subprogram bodies. Contains a
--       list of anonymous finalization masters declared within the related
--       unit. The list acts as a mapping between a master and a designated
--       type.

--    Anonymous_Object
--       Present in protected and task type entities. Contains the entity of
--       the anonymous object created for a single protected or task type.

--    Associated_Entity
--       Defined in all entities. This field is similar to Associated_Node, but
--       applied to entities. The attribute links an entity from the generic
--       template with its corresponding entity in the analyzed generic copy.
--       The global references mechanism relies on the Associated_Entity to
--       infer the context.

--    Associated_Formal_Package
--       Defined in packages that are the actuals of formal_packages. Points
--       to the entity in the declaration for the formal package.

--    Associated_Node_For_Itype
--       Defined in all type and subtype entities. Set non-Empty only for
--       Itypes. Set to point to the associated node for the Itype, i.e.
--       the node whose elaboration generated the Itype. This is used for
--       copying trees, to determine whether or not to copy an Itype, and
--       also for accessibility checks on anonymous access types. This
--       node is typically an object declaration, component declaration,
--       type or subtype declaration.

--       For an access discriminant in a type declaration, the associated_
--       node_for_itype is the corresponding discriminant specification.

--       For an access parameter it is the enclosing subprogram declaration.

--       For an access_to_protected_subprogram parameter it is the declaration
--       of the corresponding formal parameter.
--
--       Itypes have no explicit declaration, and therefore are not attached to
--       the tree: their Parent field is always empty. The Associated_Node_For_
--       Itype is the only way to determine the construct that leads to the
--       creation of a given itype entity.

--    Associated_Storage_Pool [root type only]
--       Defined in simple and general access type entities. References the
--       storage pool to be used for the corresponding collection. A value of
--       Empty means that the default pool is to be used. This is defined
--       only in the root type, since derived types must have the same pool
--       as the parent type.

--    Barrier_Function
--       Defined in protected entries and entry families. This is the
--       subprogram declaration for the body of the function that returns
--       the value of the entry barrier.

--    Base_Type (synthesized)
--       Applies to all type and subtype entities. Returns the base type of a
--       type or subtype. The base type of a type is the type itself. The base
--       type of a subtype is the type that it constrains (which is always
--       a type entity, not some other subtype). Note that in the case of a
--       subtype of a private type, it is possible for the base type attribute
--       to return a private type, even if the subtype to which it applies is
--       non-private. See also Implementation_Base_Type. Note: it is allowed to
--       apply Base_Type to other than a type, in which case it simply returns
--       the entity unchanged.

--    Block_Node
--       Defined in block entities. Points to the identifier in the
--       Block_Statement itself. Used when retrieving the block construct
--       for finalization purposes, the block entity has an implicit label
--       declaration in the enclosing declarative part, and has otherwise
--       no direct connection in the tree with the block statement. The
--       link is to the identifier (which is an occurrence of the entity)
--       and not to the block_statement itself, because the statement may
--       be rewritten, e.g. in the process of removing dead code.

--    Body_Entity
--       Defined in package and generic package entities, points to the
--       corresponding package body entity if one is present.

--    Body_Needed_For_SAL
--       Defined in package and subprogram entities that are compilation
--       units. Indicates that the source for the body must be included
--       when the unit is part of a standalone library.

--    Body_Needed_For_Inlining
--       Defined in package entities that are compilation units. Used to
--       determine whether the body unit needs to be compiled when the
--       package declaration appears in the list of units to inline. A body
--       is needed for inline processing if the unit declaration contains
--       functions that carry pragma Inline or Inline_Always, or if it
--       contains a generic unit that requires a body.
--
--    Body_References
--       Defined in abstract state entities. Contains an element list of
--       references (identifiers) that appear in a package body whose spec
--       defines the related state. If the body refines the said state, all
--       references on this list are illegal due to the visible refinement.

--    BIP_Initialization_Call
--       Defined in constants and variables whose corresponding declaration
--       is wrapped in a transient block and the inital value is provided by
--       a build-in-place function call. Contains the relocated build-in-place
--       call after the expansion has decoupled the call from the object. This
--       attribute is used by the finalization machinery to insert cleanup code
--       for all additional transient objects found in the transient block.

--    C_Pass_By_Copy [implementation base type only]
--       Defined in record types. Set if a pragma Convention for the record
--       type specifies convention C_Pass_By_Copy. This convention name is
--       treated as identical in all respects to convention C, except that
--       if it is specified for a record type, then the C_Pass_By_Copy flag
--       is set, and if a foreign convention subprogram has a formal of the
--       corresponding type, then the parameter passing mechanism will be
--       set to By_Copy (unless specifically overridden by an Import or
--       Export pragma).

--    Can_Never_Be_Null
--       This flag is defined in all entities. It is set in an object which can
--       never have a null value. Set for constant access values initialized to
--       a non-null value. This is also set for all access parameters in Ada 83
--       and Ada 95 modes, and for access parameters that explicitly exclude
--       null in Ada 2005 mode.
--
--       This is used to avoid unnecessary resetting of the Is_Known_Non_Null
--       flag for such entities. In Ada 2005 mode, this is also used when
--       determining subtype conformance of subprogram profiles to ensure
--       that two formals have the same null-exclusion status.
--
--       This is also set on some access types, e.g. the Etype of the anonymous
--       access type of a controlling formal.

--    Can_Use_Internal_Rep [base type only]
--       Defined in Access_Subprogram_Kind nodes. This flag is set by the
--       front end and used by the backend. False means that the backend
--       must represent the type in the same way as Convention-C types (and
--       other foreign-convention types). On many targets, this means that
--       the backend will use dynamically generated trampolines for nested
--       subprograms. True means that the backend can represent the type in
--       some internal way. On the aforementioned targets, this means that the
--       backend will not use dynamically generated trampolines. This flag
--       must be False if Has_Foreign_Convention is True; otherwise, the front
--       end is free to set the policy.
--
--       Setting this False in all cases corresponds to the traditional back
--       end strategy, where all access-to-subprogram types are represented the
--       same way, independent of the Convention. For further details, see also
--       Always_Compatible_Rep in Targparm.
--
--       Efficiency note: On targets that use dynamically generated
--       trampolines, False generally favors efficiency of top-level
--       subprograms, whereas True generally favors efficiency of nested
--       ones. On other targets, this flag has little or no effect on
--       efficiency. The front end should take this into account. In
--       particular, pragma Favor_Top_Level gives a hint that the flag
--       should be False.
--
--       Note: We considered using Convention-C for this purpose, but we need
--       this separate flag, because Convention-C implies that in the case of
--       P'[Unrestricted_]Access, P also have convention C. Sometimes we want
--       to have Can_Use_Internal_Rep False for an access type, but allow P to
--       have convention Ada.

--    Chars
--       Defined in all entities. This field contains an entry into the names
--       table that has the character string of the identifier, character
--       literal or operator symbol. See Namet for further details. Note that
--       throughout the processing of the front end, this name is the simple
--       unqualified name. However, just before the backend is called, a call
--       is made to Qualify_All_Entity_Names. This causes entity names to be
--       qualified using the encoding described in exp_dbug.ads, and from that
--       point (including post backend steps, e.g. cross-reference generation),
--       the entities will contain the encoded qualified names.

--    Checks_May_Be_Suppressed
--       Defined in all entities. Set if a pragma Suppress or Unsuppress
--       mentions the entity specifically in the second argument. If this flag
--       is set the global and local suppress stacks must be consulted to
--       determine if there actually is an active Suppress or Unsuppress pragma
--       that applies to the entity.

--    Class_Postconditions
--       Defined on subprogram entities. Set if the subprogram has class-wide
--       postconditions. Denotes the (and-then) expression built by merging
--       inherited class-wide postconditions with its own class-wide
--       postconditions.

--    Class_Preconditions
--       Defined on subprogram entities. Set if the subprogram has class-wide
--       preconditions. Denotes the (or-else) expression built by merging
--       inherited class-wide preconditions with its own class-wide
--       preconditions.

--    Class_Preconditions_Subprogram
--       Defined on subprogram entities. Set on subprogram helpers and also on
--       the indirect-call wrapper internally built for subprograms that have
--       class-wide preconditions. References the subprogram that has the
--       class-wide preconditions.

--    Class_Wide_Type
--       Defined in all type entities. For a tagged type or subtype, returns
--       the corresponding implicitly declared class-wide type. For a
--       class-wide type, returns itself. Set to Empty for untagged types.

--    Cloned_Subtype
--       Defined in E_Record_Subtype and E_Class_Wide_Subtype entities.
--       Each such entity can either have a Discriminant_Constraint, in
--       which case it represents a distinct type from the base type (and
--       will have a list of components and discriminants in the list headed by
--       First_Entity) or else no such constraint, in which case it will be a
--       copy of the base type.
--
--       o  Each element of the list in First_Entity is copied from the base
--          type; in that case, this field is Empty.
--
--       o  The list in First_Entity is shared with the base type; in that
--          case, this field points to that entity.
--
--       A record or classwide subtype may also be a copy of some other
--       subtype and share the entities in the First_Entity with that subtype.
--       In that case, this field points to that subtype.
--
--       For E_Class_Wide_Subtype, the presence of Equivalent_Type overrides
--       this field. Note that this field ONLY appears in subtype entities, not
--       in type entities, it is not defined, and it is an error to reference
--       Cloned_Subtype in an E_Record_Type or E_Class_Wide_Type entity.

--    Comes_From_Source
--       This flag appears on all nodes, including entities, and indicates
--       that the node was created by the scanner or parser from the original
--       source. Thus for entities, it indicates that the entity is defined
--       in the original source program.

--    Component_Alignment (special field) [base type only]
--       Defined in array and record entities. Contains a value of type
--       Component_Alignment_Kind indicating the alignment of components.
--       Set to Calign_Default normally, but can be overridden by use of
--       the Component_Alignment pragma. Note: this field is currently
--       stored in a non-standard way, see body for details.

--    Component_Bit_Offset
--       Defined in record components (E_Component, E_Discriminant). First
--       bit position of given component, computed from the first bit and
--       position values given in the component clause. A value of No_Uint
--       means that the value is not yet known. The value can be set by the
--       appearance of an explicit component clause in a record representation
--       clause, or it can be set by the front-end in package Layout, or it can
--       be set by the backend. By the time backend processing is completed,
--       this field is always set. A negative value is used to represent
--       a value which is not known at compile time, and must be computed
--       at run-time (this happens if fields of a record have variable
--       lengths). See package Repinfo for details of these values.
--
--       Note: Component_Bit_Offset is redundant with respect to the fields
--       Normalized_First_Bit and Normalized_Position, and could in principle
--       be eliminated, but it is convenient in several situations, including
--       use in the backend, to have this redundant field.

--    Component_Clause
--       Defined in record components and discriminants. If a record
--       representation clause is present for the corresponding record type a
--       that specifies a position for the component, then the Component_Clause
--       field of the E_Component entity points to the N_Component_Clause node.
--       Set to Empty if no record representation clause was present, or if
--       there was no specification for this component.

--    Component_Size [implementation base type only]
--       Defined in array types. It contains the component size value for
--       the array. A value of No_Uint means that the value is not yet set.
--       The value can be set by the use of a component size clause, or
--       by the front end in package Layout, or by the backend. A negative
--       value is used to represent a value which is not known at compile
--       time, and must be computed at run-time (this happens if the type
--       of the component has a variable length size). See package Repinfo
--       for details of these values. Component_Size can also be negative in
--       an illegal program that says e.g. "for T'Component_Size use -8;".

--    Component_Type [implementation base type only]
--       Defined in array types and string types. References component type.

--    Contains_Ignored_Ghost_Code
--       Defined in blocks, packages and their bodies, subprograms and their
--       bodies. Set if the entity contains any ignored Ghost code in the form
--       of declaration, procedure call, assignment statement or pragma.

--    Contract
--       Defined in constant, entry, entry family, operator, [generic] package,
--       package body, protected unit, [generic] subprogram, subprogram body,
--       variable, task unit, and type entities. Points to the contract of the
--       entity, holding various assertion items and data classifiers.

--    Contract_Wrapper
--       Defined in entry and entry family entities. Set only when the entry
--       [family] has contract cases, preconditions, and/or postconditions.
--       Contains the entity of a wrapper procedure which encapsulates the
--       original entry and implements precondition/postcondition semantics.

--    Corresponding_Concurrent_Type
--       Defined in record types that are constructed by the expander to
--       represent task and protected types (Is_Concurrent_Record_Type flag
--       set). Points to the entity for the corresponding task type or the
--       protected type.

--    Corresponding_Discriminant
--       Defined in discriminants of a derived type, when the discriminant is
--       used to constrain a discriminant of the parent type. Points to the
--       corresponding discriminant in the parent type. Otherwise it is Empty.

--    Corresponding_Equality
--       Defined in function entities for implicit inequality operators.
--       Denotes the explicit or derived equality operation that creates
--       the implicit inequality. Note that this field is not present in
--       other function entities, only in implicit inequality routines,
--       where Comes_From_Source is always False.

--    Corresponding_Function
--       Defined on procedures internally built with an extra out parameter
--       to return a constrained array type, when Modify_Tree_For_C is set.
--       Denotes the function that returns the constrained array type for
--       which this procedure was built.

--    Corresponding_Procedure
--       Defined on functions that return a constrained array type, when
--       Modify_Tree_For_C is set. Denotes the internally built procedure
--       with an extra out parameter created for it.

--    Corresponding_Record_Component
--       Defined in components of a derived untagged record type, including
--       discriminants. For a regular component or a stored discriminant,
--       points to the corresponding component in the parent type. Set to
--       Empty for a non-stored discriminant. It is used by the back end to
--       ensure the layout of the derived type matches that of the parent
--       type when there is no representation clause on the derived type.

--    Corresponding_Record_Type
--       Defined in protected and task types and subtypes. References the
--       entity for the corresponding record type constructed by the expander
--       (see Exp_Ch9). This type is used to represent values of the task type.

--    Corresponding_Remote_Type
--       Defined in record types that describe the fat pointer structure for
--       Remote_Access_To_Subprogram types. References the original access
--       to subprogram type.

--    CR_Discriminant
--       Defined in discriminants of concurrent types. Denotes the homologous
--       discriminant of the corresponding record type. The CR_Discriminant is
--       created at the same time as the discriminal, and used to replace
--       occurrences of the discriminant within the type declaration.

--    Current_Use_Clause
--       Defined in packages and in types. For packages, denotes the use
--       package clause currently in scope that makes the package use_visible.
--       For types, it denotes the use_type clause that makes the operators of
--       the type visible. Used for more precise warning messages on redundant
--       use clauses.

--    Current_Value
--       Defined in all object entities. Set in E_Variable, E_Constant, formal
--       parameters and E_Loop_Parameter entities if we have trackable current
--       values. Set non-Empty if the (constant) current value of the variable
--       is known. This value is valid only for references from the same
--       sequential scope as the entity. The sequential scope of an entity
--       includes the immediate scope and any contained scopes that are package
--       specs, package bodies, blocks (at any nesting level) or statement
--       sequences in IF or loop statements.
--
--       Another related use of this field is to record information about the
--       value obtained from an IF or WHILE statement condition. If the IF or
--       ELSIF or WHILE condition has the form "NOT {,NOT] OBJ RELOP VAL ",
--       or OBJ [AND [THEN]] expr, where OBJ refers to an entity with a
--       Current_Value field, RELOP is one of the six relational operators, and
--       VAL is a compile-time known value then the Current_Value field of OBJ
--       points to the N_If_Statement, N_Elsif_Part, or N_Iteration_Scheme node
--       of the relevant construct, and the Condition field of this can be
--       consulted to give information about the value of OBJ. For more details
--       on this usage, see the procedure Exp_Util.Get_Current_Value_Condition.

--    Debug_Info_Off
--       Defined in all entities. Set if a pragma Suppress_Debug_Info applies
--       to the entity, or if internal processing in the compiler determines
--       that suppression of debug information is desirable. Note that this
--       flag is only for use by the front end as part of the processing for
--       determining if Needs_Debug_Info should be set. The backend should
--       always test Needs_Debug_Info, it should never test Debug_Info_Off.

--    Debug_Renaming_Link
--       Used to link the variable associated with a debug renaming declaration
--       to the renamed entity. See Exp_Dbug.Debug_Renaming_Declaration for
--       details of the use of this field.

--    Declaration_Node (synthesized)
--       Applies to all entities. Returns the tree node for the construct that
--       declared the entity. Normally this is just the Parent of the entity.
--       One exception arises with child units, where the parent of the entity
--       is a selected component/defining program unit name. Another exception
--       is that if the entity is an incomplete type that has been completed or
--       a private type, then we obtain the declaration node denoted by the
--       full type, i.e. the full type declaration node. Also note that for
--       subprograms, this returns the {function,procedure}_specification, not
--       the subprogram_declaration. If the parent of an Itype is a type or
--       subtype declaration, we return the declaration node as for any other
--       type. For other Itypes, we return Empty.

--    Default_Aspect_Component_Value [base type only]
--       Defined in array types. Holds the static value specified in a
--       Default_Component_Value aspect specification for the array type,
--       or inherited on derivation.

--    Default_Aspect_Value [base type only]
--       Defined in scalar types. Holds the static value specified in a
--       Default_Value aspect specification for the type, or inherited
--       on derivation.

--    Default_Expr_Function
--       Defined in parameters. It holds the entity of the parameterless
--       function that is built to evaluate the default expression if it is
--       more complex than a simple identifier or literal. For the latter
--       simple cases or if there is no default value, this field is Empty.

--    Default_Expressions_Processed
--       A flag in subprograms (functions, operators, procedures) and in
--       entries and entry families used to indicate that default expressions
--       have been processed and to avoid multiple calls to process the
--       default expressions (see Freeze.Process_Default_Expressions), which
--       would not only waste time, but also generate false error messages.

--    Default_Value
--       Defined in formal parameters. Points to the node representing the
--       expression for the default value for the parameter. Empty if the
--       parameter has no default value (which is always the case for OUT
--       and IN OUT parameters in the absence of errors).

--    Delay_Cleanups
--       Defined in entities that have finalization lists (subprograms
--       blocks, and tasks). Set if there are pending generic body
--       instantiations for the corresponding entity. If this flag is
--       set, then generation of cleanup actions for the corresponding
--       entity must be delayed, since the insertion of the generic body
--       may affect cleanup generation (see Inline for further details).

--    Delay_Subprogram_Descriptors
--       Defined in entities for which exception subprogram descriptors
--       are generated (subprograms, package declarations and package
--       bodies). Defined if there are pending generic body instantiations
--       for the corresponding entity. If this flag is set, then generation
--       of the subprogram descriptor for the corresponding entities must
--       be delayed, since the insertion of the generic body may add entries
--       to the list of handlers.
--
--       Note: for subprograms, Delay_Subprogram_Descriptors is set if and
--       only if Delay_Cleanups is set. But Delay_Cleanups can be set for a
--       a block (in which case Delay_Subprogram_Descriptors is set for the
--       containing subprogram). In addition Delay_Subprogram_Descriptors is
--       set for a library level package declaration or body which contains
--       delayed instantiations (in this case the descriptor refers to the
--       enclosing elaboration procedure).

--    Delta_Value
--       Defined in fixed and decimal types. Points to a universal real
--       that holds value of delta for the type, as given in the declaration
--       or as inherited by a subtype or derived type.

--    Dependent_Instances
--       Defined in packages that are instances. Holds list of instances
--       of inner generics. Used to place freeze nodes for those instances
--       after that of the current one, i.e. after the corresponding generic
--       bodies.

--    Depends_On_Private
--       Defined in all type entities. Set if the type is private or if it
--       depends on a private type.

--    Derived_Type_Link
--       Defined in all type and subtype entities. Set in a base type if
--       a derived type declaration is encountered which derives from
--       this base type or one of its subtypes, and there are already
--       primitive operations declared. In this case, it references the
--       entity for the type declared by the derived type declaration.
--       For example:
--
--          type R is ...
--          subtype RS is R ...
--          ...
--          type G is new RS ...
--
--       In this case, if primitive operations have been declared for R, at
--       the point of declaration of G, then the Derived_Type_Link of R is set
--       to point to the entity for G. This is used to generate warnings and
--       errors for rep clauses that appear later on for R, which might result
--       in an unexpected (or illegal) implicit conversion operation.
--
--       Note: if there is more than one such derived type, the link will point
--       to the last one.

--    Designated_Type (synthesized)
--       Applies to access types. Returns the designated type. Differs from
--       Directly_Designated_Type in that if the access type refers to an
--       incomplete type, and the full type is available, then this full type
--       is returned instead of the incomplete type.

--    DIC_Procedure (synthesized)
--       Defined in all type entities. Set for a private type and its full view
--       when the type is subject to pragma Default_Initial_Condition (DIC), or
--       when the type inherits a DIC pragma from a parent type. Points to the
--       entity of a procedure which takes a single argument of the given type
--       and verifies the assertion expression of the DIC pragma at run time.

--       Note: the reason this is marked as a synthesized attribute is that the
--       way this is stored is as an element of the Subprograms_For_Type field.

--    Digits_Value
--       Defined in floating point types and subtypes and decimal types and
--       subtypes. Contains the Digits value specified in the declaration.

--    Direct_Primitive_Operations
--       Defined in tagged types and subtypes (including synchronized types),
--       in tagged private types, and in tagged incomplete types. Moreover, it
--       is also defined for untagged types, both when Extensions_Allowed is
--       True (-gnatX) to support the extension feature of prefixed calls for
--       untagged types, and when Extensions_Allowed is False to get better
--       error messages. This field is an element list of entities for
--       primitive operations of the type. For incomplete types the list is
--       always empty. In order to follow the C++ ABI, entities of primitives
--       that come from source must be stored in this list in the order of
--       their occurrence in the sources. When expansion is disabled, the
--       corresponding record type of a synchronized type is not constructed.
--       In that case, such types carry this attribute directly.

--    Directly_Designated_Type
--       Defined in access types. This field points to the type that is
--       directly designated by the access type. In the case of an access
--       type to an incomplete type, this field references the incomplete
--       type. Directly_Designated_Type is typically used in implementing the
--       static semantics of the language; in implementing dynamic semantics,
--       we typically want the full view of the designated type. The function
--       Designated_Type obtains this full type in the case of access to an
--       incomplete type.

--    Disable_Controlled [base type only]
--      Present in all entities. Set for a controlled type subject to aspect
--      Disable_Controlled which evaluates to True. This flag is taken into
--      account in synthesized attribute Is_Controlled.

--    Discard_Names
--       Defined in types and exception entities. Set if pragma Discard_Names
--       applies to the entity. It is also set for declarative regions and
--       package specs for which a Discard_Names pragma with zero arguments
--       has been encountered. The purpose of setting this flag is to be able
--       to set the Discard_Names attribute on enumeration types declared
--       after the pragma within the same declarative region. This flag is
--       set to False if a Keep_Names pragma appears for an enumeration type.

--    Discriminal
--       Defined in discriminants (Discriminant formal: GNAT's first
--       coinage). The entity used as a formal parameter that corresponds
--       to a discriminant. See section "Handling of Discriminants" for
--       full details of the use of discriminals.

--    Discriminal_Link
--       Defined in E_In_Parameter or E_Constant entities. For discriminals,
--       points back to corresponding discriminant. For other entities, must
--       remain Empty.

--    Discriminant_Checking_Func
--       Defined in components. Points to the defining identifier of the
--       function built by the expander returns a Boolean indicating whether
--       the given record component exists for the current discriminant
--       values.

--    Discriminant_Constraint
--       Defined in entities whose Has_Discriminants flag is set (concurrent
--       types, subtypes, record types and subtypes, private types and
--       subtypes, limited private types and subtypes and incomplete types).
--       It is an error to reference the Discriminant_Constraint field if
--       Has_Discriminants is False.
--
--       If the Is_Constrained flag is set, Discriminant_Constraint points
--       to an element list containing the discriminant constraints in the
--       same order in which the discriminants are declared.
--
--       If the Is_Constrained flag is not set but the discriminants of the
--       unconstrained type have default initial values then this field
--       points to an element list giving these default initial values in
--       the same order in which the discriminants are declared. Note that
--       in this case the entity cannot be a tagged record type, because
--       discriminants in this case cannot have defaults.
--
--       If the entity is a tagged record implicit type, then this field is
--       inherited from the first subtype (so that the itype is subtype
--       conformant with its first subtype, which is needed when the first
--       subtype overrides primitive operations inherited by the implicit
--       base type).
--
--       In all other cases Discriminant_Constraint contains the empty
--       Elist (i.e. it is initialized with a call to New_Elmt_List).

--    Discriminant_Default_Value
--       Defined in discriminants. Points to the node representing the
--       expression for the default value of the discriminant. Set to
--       Empty if the discriminant has no default value.

--    Discriminant_Number
--       Defined in discriminants. Gives the ranking of a discriminant in
--       the list of discriminants of the type, i.e. a sequential integer
--       index starting at 1 and ranging up to number of discriminants.

--    Dispatch_Table_Wrappers [implementation base type only]
--       Defined in E_Record_Type and E_Record_Subtype entities. Set in library
--       level tagged type entities if we are generating statically allocated
--       dispatch tables. Points to the list of dispatch table wrappers
--       associated with the tagged type. For an untagged record, contains
--       No_Elist.

--    Dynamic_Call_Helper
--       Defined on subprogram entities. Set if the subprogram has class-wide
--       preconditions. Denotes the helper that evaluates at run time the
--       class-wide preconditions performing dispatching calls.

--    DTC_Entity
--       Defined in function and procedure entities. Set to Empty unless
--       the subprogram is dispatching in which case it references the
--       Dispatch Table pointer Component. For regular Ada tagged this, this
--       is the _Tag component. For CPP_Class types and their descendants,
--       this points to the component entity in the record that holds the
--       Vtable pointer for the Vtable containing the entry referencing the
--       subprogram.

--    DT_Entry_Count
--       Defined in E_Component entities. Only used for component marked
--       Is_Tag. Store the number of entries in the Vtable (or Dispatch Table)

--    DT_Offset_To_Top_Func
--       Defined in E_Component entities. Only used for component marked
--       Is_Tag. If present it stores the Offset_To_Top function used to
--       provide this value in tagged types whose ancestor has discriminants.

--    DT_Position
--       Defined in function and procedure entities which are dispatching
--       (should not be referenced without first checking that flag
--       Is_Dispatching_Operation is True). Contains the offset into
--       the Vtable for the entry that references the subprogram.

--    Ekind (Ekind)
--       Defined in all entities. Contains a value of the enumeration type
--       Entity_Kind declared in a subsequent section in this spec.

--    Elaborate_Body_Desirable
--       Defined in package entities. Set if the elaboration circuitry detects
--       a case where there is a package body that modifies one or more visible
--       entities in the package spec and there is no explicit Elaborate_Body
--       pragma for the package. This information is passed on to the binder,
--       which attempts, but does not promise, to elaborate the body as close
--       to the spec as possible.

--    Elaboration_Entity
--       Defined in entry, entry family, [generic] package, and subprogram
--       entities. This is a counter associated with the unit that is initially
--       set to zero, is incremented when an elaboration request for the unit
--       is made, and is decremented when a finalization request for the unit
--       is made. This is used for three purposes. First, it is used to
--       implement access before elaboration checks (the counter must be
--       non-zero to call a subprogram at elaboration time). Second, it is
--       used to guard against repeated execution of the elaboration code.
--       Third, it is used to ensure that the finalization code is executed
--       only after all clients have requested it.
--
--       Note that we always allocate this counter, and set this field, but
--       we do not always actually use it. It is only used if it is needed
--       for access before elaboration use (see Elaboration_Entity_Required
--       flag) or if either the spec or the body has elaboration code. If
--       neither of these two conditions holds, then the entity is still
--       allocated (since we don't know early enough whether or not there
--       is elaboration code), but is simply not used for any purpose.

--    Elaboration_Entity_Required
--       Defined in entry, entry family, [generic] package, and subprogram
--       entities. Set only if Elaboration_Entity is non-Empty to indicate that
--       the counter is required to be non-zero even if there is no other
--       elaboration code. This occurs when the Elaboration_Entity counter
--       is used for access before elaboration checks. If the counter is
--       only used to prevent multiple execution of the elaboration code,
--       then if there is no other elaboration code, obviously there is no
--       need to set the flag.

--    Encapsulating_State
--       Defined in abstract state, constant and variable entities. Contains
--       the entity of an ancestor state or a single concurrent type whose
--       refinement utilizes this item as a constituent.

--    Enclosing_Scope
--       Defined in labels. Denotes the innermost enclosing construct that
--       contains the label. Identical to the scope of the label, except for
--       labels declared in the body of an accept statement, in which case the
--       entry_name is the Enclosing_Scope. Used to validate goto's within
--       accept statements.

--    Entry_Accepted
--       Defined in E_Entry and E_Entry_Family entities. Set if there is
--       at least one accept for this entry in the task body. Used to
--       generate warnings for missing accepts.

--    Entry_Bodies_Array
--       Defined in protected types for which Has_Entries is true.
--       This is the defining identifier for the array of entry body
--       action procedures and barrier functions used by the runtime to
--       execute the user code associated with each entry.

--    Entry_Cancel_Parameter
--       Defined in blocks. This only applies to a block statement for
--       which the Is_Asynchronous_Call_Block flag is set. It
--       contains the defining identifier of an object that must be
--       passed to the Cancel_Task_Entry_Call or Cancel_Protected_Entry_Call
--       call in the cleanup handler added to the block by
--       Exp_Ch7.Expand_Cleanup_Actions. This parameter is a Boolean
--       object for task entry calls and a Communications_Block object
--       in the case of protected entry calls. In both cases the objects
--       are declared in outer scopes to this block.

--    Entry_Component
--       Defined in formal parameters (in, in out and out parameters). Used
--       only for formals of entries. References the corresponding component
--       of the entry parameter record for the entry.

--    Entry_Formal
--       Defined in components of the record built to correspond to entry
--       parameters. This field points from the component to the formal. It
--       is the back pointer corresponding to Entry_Component.

--    Entry_Index_Constant
--       Defined in an entry index parameter. This is an identifier that
--       eventually becomes the name of a constant representing the index
--       of the entry family member whose entry body is being executed. Used
--       to expand references to the entry index specification identifier.

--    Entry_Index_Type (synthesized)
--       Applies to an entry family. Denotes Etype of the subtype indication
--       in the entry declaration. Used to resolve the index expression in an
--       accept statement for a member of the family, and in the prefix of
--       'COUNT when it applies to a family member.

--    Entry_Max_Queue_Lengths_Array
--       Defined in protected types for which Has_Entries is true. Contains the
--       defining identifier for the array of naturals used by the runtime to
--       limit the queue size of each entry individually.

--    Entry_Parameters_Type
--       Defined in entries. Points to the access-to-record type that is
--       constructed by the expander to hold a reference to the parameter
--       values. This reference is manipulated (as an address) by the
--       tasking runtime. The designated record represents a packaging
--       up of the entry parameters (see Exp_Ch9.Expand_N_Entry_Declaration
--       for further details). Entry_Parameters_Type is Empty if the entry
--       has no parameters.

--    Enumeration_Pos
--       Defined in enumeration literals. Contains the position number
--       corresponding to the value of the enumeration literal.

--    Enumeration_Rep
--       Defined in enumeration literals. Contains the representation that
--       corresponds to the value of the enumeration literal. Note that
--       this is normally the same as Enumeration_Pos except in the presence
--       of representation clauses, where Pos will still represent the
--       position of the literal within the type and Rep will have be the
--       value given in the representation clause.

--    Enumeration_Rep_Expr
--       Defined in enumeration literals. Points to the expression in an
--       associated enumeration rep clause that provides the representation
--       value for this literal. Empty if no enumeration rep clause for this
--       literal (or if rep clause does not have an entry for this literal,
--       an error situation). This is also used to catch duplicate entries
--       for the same literal.

--    Enum_Pos_To_Rep
--       Defined in enumeration types, but not enumeration subtypes. Set to
--       Empty unless the enumeration type has a non-standard representation,
--       i.e. at least one literal has a representation value different from
--       its position value. In this case, the alternative is the following:
--       if the representation is not contiguous, then Enum_Pos_To_Rep is the
--       entity for an array constant built when the type is frozen that maps
--       Pos values to corresponding Rep values, whose index type is Natural
--       and whose component type is the enumeration type itself; or else, if
--       the representation is contiguous, then Enum_Pos_To_Rep is the entity
--       of the index type defined above.

--    Equivalent_Type
--       Defined in class wide types and subtypes, access to protected
--       subprogram types, and in exception types. For a classwide type, it
--       is always Empty. For a class wide subtype, it points to an entity
--       created by the expander which gives the backend an understandable
--       equivalent of the class subtype with a known size (given by an
--       initial value). See Exp_Util.Expand_Class_Wide_Subtype for further
--       details. For E_Exception_Type, this points to the record containing
--       the data necessary to represent exceptions (for further details, see
--       System.Standard_Library). For access to protected subprograms, it
--       denotes a record that holds pointers to the operation and to the
--       protected object. For remote Access_To_Subprogram types, it denotes
--       the record that is the fat pointer representation of an RAST.

--    Esize
--       Defined in all types and subtypes, and also for components, constants,
--       and variables, including exceptions where it refers to the static data
--       allocated for an exception. Contains the Object_Size of the type or of
--       the object. A value of zero indicates that the value is not yet known.
--
--       For the case of components where a component clause is present, the
--       value is the value from the component clause, which must be non-
--       negative (but may be zero, which is acceptable for the case of
--       a type with only one possible value). It is also possible for Esize
--       of a component to be set without a component clause defined, which
--       means that the component size is specified, but not the position.
--       See also RM_Size and the section on "Handling of Type'Size Values".
--       During backend processing, the value is back annotated for all zero
--       values, so that after the call to the backend, the value is set.

--    Etype
--       Defined in all entities. Represents the type of the entity, which
--       is itself another entity. For a type entity, points to the parent
--       type for a derived type, or if the type is not derived, points to
--       itself. For a subtype entity, Etype points to the base type. For
--       a class wide type, points to the corresponding specific type. For a
--       subprogram or subprogram type, Etype has the return type of a function
--       or is set to Standard_Void_Type to represent a procedure. The Etype
--       field of a package is also set to Standard_Void_Type.
--
--       Note one obscure case: for pragma Default_Storage_Pool (null), the
--       Etype of the N_Null node is Empty.

--    Extra_Accessibility
--       Defined in formal parameters in the non-generic case. Normally Empty,
--       but if expansion is active, and a parameter is one for which a
--       dynamic accessibility check is required, then an extra formal of type
--       Natural is created (see description of field Extra_Formal), and the
--       Extra_Accessibility field of the formal parameter points to the entity
--       for this extra formal. Also defined in variables when compiling
--       receiving stubs. In this case, a non Empty value means that this
--       variable's accessibility depth has been transmitted by the caller and
--       must be retrieved through the entity designed by this field instead of
--       being computed.

--    Extra_Accessibility_Of_Result
--       Defined in (non-generic) Function, Operator, and Subprogram_Type
--       entities. Normally Empty, but if expansion is active, and a function
--       is one for which "the accessibility level of the result ... determined
--       by the point of call" (AI05-0234) is needed, then an extra formal of
--       subtype Natural is created (see description of field Extra_Formal),
--       and the Extra_Accessibility_Of_Result field of the function points to
--       the entity for this extra formal.

--    Extra_Constrained
--       Defined in formal parameters in the non-generic case. Normally Empty,
--       but if expansion is active and a parameter is one for which a dynamic
--       indication of its constrained status is required, then an extra formal
--       of type Boolean is created (see description of field Extra_Formal),
--       and the Extra_Constrained field of the formal parameter points to the
--       entity for this extra formal. Also defined in variables when compiling
--       receiving stubs. In this case, a non empty value means that this
--       variable's constrained status has been transmitted by the caller and
--       must be retrieved through the entity designed by this field instead of
--       being computed.

--    Extra_Formal
--       Defined in formal parameters in the non-generic case. Certain
--       parameters require extra implicit information to be passed (e.g. the
--       flag indicating if an unconstrained variant record argument is
--       constrained, and the accessibility level for access parameters). See
--       description of Extra_Constrained, Extra_Accessibility fields for
--       further details. Extra formal parameters are constructed to represent
--       these values, and chained to the end of the list of formals using the
--       Extra_Formal field (i.e. the Extra_Formal field of the last "real"
--       formal points to the first extra formal, and the Extra_Formal field of
--       each extra formal points to the next one, with Empty indicating the
--       end of the list of extra formals). Another case of Extra_Formal arises
--       in connection with unnesting of subprograms, where the ARECnF formal
--       that represents an activation record pointer is an extra formal.

--    Extra_Formals
--       Applies to subprograms, subprogram types, entries, and entry
--       families. Returns first extra formal of the subprogram or entry.
--       Returns Empty if there are no extra formals.

--    Finalization_Master [root type only]
--       Defined in access-to-controlled or access-to-class-wide types. The
--       field contains the entity of the finalization master which handles
--       dynamically allocated controlled objects referenced by the access
--       type. Empty for access-to-subprogram types. Empty for access types
--       whose designated type does not need finalization actions.

--    Finalize_Storage_Only [base type only]
--       Defined in all types. Set on direct controlled types to which a
--       valid Finalize_Storage_Only pragma applies. This flag is also set on
--       composite types when they have at least one controlled component and
--       all their controlled components are Finalize_Storage_Only. It is also
--       inherited by type derivation except for direct controlled types where
--       the Finalize_Storage_Only pragma is required at each level of
--       derivation.

--    Finalizer
--       Applies to package declarations and bodies. Contains the entity of the
--       library-level program which finalizes all package-level controlled
--       objects.

--    First_Component (synthesized)
--       Applies to incomplete, private, protected, record and task types.
--       Returns the first component by following the chain of declared
--       entities for the type a component is found (one with an Ekind of
--       E_Component). The discriminants are skipped. If the record is null,
--       then Empty is returned.

--    First_Component_Or_Discriminant (synthesized)
--       Similar to First_Component, but discriminants are not skipped, so will
--       find the first discriminant if discriminants are present.

--    First_Entity
--       Defined in all entities which act as scopes to which a list of
--       associated entities is attached (blocks, class subtypes and types,
--       entries, functions, loops, packages, procedures, protected objects,
--       record types and subtypes, private types, task types and subtypes).
--       Points to a list of associated entities using the Next_Entity field
--       as a chain pointer with Empty marking the end of the list.

--    First_Exit_Statement
--       Defined in E_Loop entity. The exit statements for a loop are chained
--       (in reverse order of appearance) using this field to point to the
--       first entry in the chain (last exit statement in the loop). The
--       entries are chained through the Next_Exit_Statement field of the
--       N_Exit_Statement node with Empty marking the end of the list.

--    First_Formal (synthesized)
--       Applies to subprograms and subprogram types, and also to entries
--       and entry families. Returns first formal of the subprogram or entry.
--       The formals are the first entities declared in a subprogram or in
--       a subprogram type (the designated type of an Access_To_Subprogram
--       definition) or in an entry.

--    First_Formal_With_Extras (synthesized)
--       Applies to subprograms and subprogram types, and also in entries
--       and entry families. Returns first formal of the subprogram or entry.
--       Returns Empty if there are no formals. The list returned includes
--       all the extra formals (see description of Extra_Formals field).

--    First_Index
--       Defined in array types and subtypes. By introducing implicit subtypes
--       for the index constraints, we have the same structure for constrained
--       and unconstrained arrays, subtype marks and discrete ranges are
--       both represented by a subtype. This function returns the tree node
--       corresponding to an occurrence of the first index (NOT the entity for
--       the type). Subsequent indices are obtained using Next_Index. Note that
--       this field is defined for the case of string literal subtypes, but is
--       always Empty.

--    First_Literal
--       Defined in all enumeration types, including character and boolean
--       types. This field points to the first enumeration literal entity
--       for the type (i.e. it is set to First (Literals (N)) where N is
--       the enumeration type definition node. A special case occurs with
--       standard character and wide character types, where this field is
--       Empty, since there are no enumeration literal lists in these cases.
--       Note that this field is set in enumeration subtypes, but it still
--       points to the first literal of the base type in this case.

--    First_Private_Entity
--       Defined in all entities containing private parts (packages, protected
--       types and subtypes, task types and subtypes). The entities on the
--       entity chain are in order of declaration, so the entries for private
--       entities are at the end of the chain. This field points to the first
--       entity for the private part. It is Empty if there are no entities
--       declared in the private part or if there is no private part.

--    First_Rep_Item
--       Defined in all entities. If non-empty, points to a linked list of
--       representation pragmas nodes and representation clause nodes that
--       apply to the entity, linked using Next_Rep_Item, with Empty marking
--       the end of the list. In the case of derived types and subtypes, the
--       new entity inherits the chain at the point of declaration. This means
--       that it is possible to have multiple instances of the same kind of rep
--       item on the chain, in which case it is the first one that applies to
--       the entity.
--
--       Note: pragmas that can apply to more than one overloadable entity,
--       (Convention, Interface, Inline, Inline_Always, Import, Export,
--       External) are never present on this chain when they apply to
--       overloadable entities, since it is impossible for a given pragma
--       to be on more than one chain at a time.
--
--       For most representation items, the representation information is
--       reflected in other fields and flags in the entity. For example if a
--       record representation clause is present, the component entities
--       reflect the specified information. However, there are some items that
--       are only reflected in the chain. These include:
--
--          Machine_Attribute pragma
--          Link_Alias pragma
--          Linker_Constructor pragma
--          Linker_Destructor pragma
--          Weak_External pragma
--          Thread_Local_Storage pragma
--
--       If any of these items are present, then the flag Has_Gigi_Rep_Item is
--       set, indicating that the backend should search the chain.
--
--       Other representation items are included in the chain so that error
--       messages can easily locate the relevant nodes for posting errors.
--       Note in particular that size clauses are defined only for this
--       purpose, and should only be accessed if Has_Size_Clause is set.

--    Float_Rep [base type only]
--       Defined in floating-point entities. Contains a value of type
--       Float_Rep_Kind. Together with the Digits_Value uniquely defines
--       the floating-point representation to be used.

--    Freeze_Node
--       Defined in all entities. If there is an associated freeze node for the
--       entity, this field references this freeze node. If no freeze node is
--       associated with the entity, then this field is Empty. See package
--       Freeze for further details.

--    From_Limited_With
--       Defined in abtract states, package and type entities. Set to True when
--       the related entity is generated by the expansion of a limited with
--       clause. Such an entity is said to be a "shadow" - it acts as the
--       abstract view of a state or variable or as the incomplete view of a
--       type by inheriting relevant attributes from the said entity.

--    Full_View
--       Defined in all type and subtype entities and in deferred constants.
--       References the entity for the corresponding full type or constant
--       declaration. For all types other than private and incomplete types,
--       this field always contains Empty. If an incomplete type E1 is
--       completed by a private type E2 whose full type declaration entity is
--       E3 then the full view of E1 is E2, and the full view of E2 is E3. See
--       also Underlying_Type.

--    Generic_Homonym
--       Defined in generic packages. The generic homonym is the entity of
--       a renaming declaration inserted in every generic unit. It is used
--       to resolve the name of a local entity that is given by a qualified
--       name, when the generic entity itself is hidden by a local name.

--    Generic_Renamings
--       Defined in package and subprogram instances. Holds mapping that
--       associates generic parameters with the corresponding instances, in
--       those cases where the instance is an entity.

--    Handler_Records
--       Defined in subprogram and package entities. Points to a list of
--       identifiers referencing the handler record entities for the
--       corresponding unit.

--    Has_Aliased_Components [implementation base type only]
--       Defined in array type entities. Indicates that the component type
--       of the array is aliased. Should this also be set for records to
--       indicate that at least one component is aliased (see processing in
--       Sem_Prag.Process_Atomic_Independent_Shared_Volatile???)

--    Has_Alignment_Clause
--       Defined in all type entities and objects. Indicates if an alignment
--       clause has been given for the entity. If set, then Alignment_Clause
--       returns the N_Attribute_Definition node for the alignment attribute
--       definition clause. Note that it is possible for this flag to be False
--       even when Alignment_Clause returns non_Empty (this happens in the case
--       of derived type declarations).

--    Has_All_Calls_Remote
--       Defined in all library unit entities. Set if the library unit has an
--       All_Calls_Remote pragma. Note that such entities must also be RCI
--       entities, so the flag Is_Remote_Call_Interface will always be set if
--       this flag is set.

--    Has_Atomic_Components [implementation base type only]
--       Defined in all types and objects. Set only for an array type or
--       an array object if a valid pragma Atomic_Components applies to the
--       type or object. Note that in the case of an object, this flag is
--       only set on the object if there was an explicit pragma for the
--       object. In other words, the proper test for whether an object has
--       atomic components is to see if either the object or its base type
--       has this flag set. Note that in the case of a type, the pragma will
--       be chained to the rep item chain of the first subtype in the usual
--       manner.

--    Has_Attach_Handler (synthesized)
--       Applies to record types that are constructed by the expander to
--       represent protected types. Returns True if there is at least one
--       Attach_Handler pragma in the corresponding specification.

--    Has_Biased_Representation
--       Defined in discrete types (where it applies to the type'size value),
--       and to objects (both stand-alone and components), where it applies to
--       the size of the object from a size or record component clause. In
--       all cases it indicates that the size in question is smaller than
--       would normally be required, but that the size requirement can be
--       satisfied by using a biased representation, in which stored values
--       have the low bound (Expr_Value (Type_Low_Bound (T)) subtracted to
--       reduce the required size. For example, a type with a range of 1..2
--       takes one bit, using 0 to represent 1 and 1 to represent 2.
--
--       Note that in the object and component cases, the flag is only set if
--       the type is unbiased, but the object specifies a smaller size than the
--       size of the type, forcing biased representation for the object, but
--       the subtype is still an unbiased type.

--    Has_Completion
--       Defined in all entities that require a completion (functions,
--       procedures, private types, limited private types, incomplete types,
--       constants and packages that require a body). The flag is set if the
--       completion has been encountered and analyzed.

--    Has_Completion_In_Body
--       Defined in all entities for types and subtypes. Set only in "Taft
--       amendment types" (incomplete types whose full declaration appears in
--       the package body).

--    Has_Complex_Representation [implementation base type only]
--       Defined in record types. Set only for a base type to which a valid
--       pragma Complex_Representation applies.

--    Has_Component_Size_Clause [implementation base type only]
--       Defined in all type entities. Set if a component size clause is
--       Defined for the given type. Note that this flag can be False even
--       if Component_Size is non-zero (happens in the case of derived types).

--    Has_Constrained_Partial_View [base type only]
--       Defined in private type and their completions, when the private
--       type has no discriminants and the full view has discriminants with
--       defaults. In Ada 2005 heap-allocated objects of such types are not
--       constrained, and can change their discriminants with full assignment.
--
--       Ada 2012 has an additional rule (3.3. (23/10.3)) concerning objects
--       declared in a generic package body. Objects whose type is an untagged
--       generic formal private type are considered to have a constrained
--       partial view. The predicate Object_Type_Has_Constrained_Partial_View
--       in sem_aux is used to test for this case.

--    Has_Contiguous_Rep
--       Defined in enumeration types. Set if the type has a representation
--       clause whose entries are successive integers.

--    Has_Controlled_Component [base type only]
--       Defined in all type and subtype entities. Set only for composite type
--       entities which contain a component that either is a controlled type,
--       or itself contains controlled component (i.e. either Is_Controlled or
--       Has_Controlled_Component is set for at least one component).

--    Has_Controlling_Result
--       Defined in E_Function entities. Set if the function is a primitive
--       function of a tagged type which can dispatch on result. Also set on
--       secondary stack thunks built for such a primitive function.

--    Has_Convention_Pragma
--       Defined in all entities. Set for an entity for which a valid pragma
--       Convention, Import, or Export has been given. Used to prevent more
--       than one such pragma appearing for a given entity (RM B.1(45)).

--    Has_Default_Aspect [base type only]
--       Defined in entities for types and subtypes, set for scalar types with
--       a Default_Value aspect and array types with a Default_Component_Value
--       aspect. If this flag is set, then a corresponding aspect specification
--       node will be present on the rep item chain for the entity. For a
--       derived type that inherits a default from its ancestor, the default
--       value is set, but it may be overridden by an aspect declaration on
--       type derivation.

--    Has_Delayed_Aspects
--      Defined in all entities. Set if the Rep_Item chain for the entity has
--      one or more N_Aspect_Definition nodes chained which are not to be
--      evaluated till the freeze point. The aspect definition expression
--      clause has been preanalyzed to get visibility at the point of use,
--      but no other action has been taken.

--    Has_Delayed_Freeze
--       Defined in all entities. Set to indicate that an explicit freeze
--       node must be generated for the entity at its freezing point. See
--       separate section ("Delayed Freezing and Elaboration") for details.

--    Has_Delayed_Rep_Aspects
--       Defined in all types and subtypes. This flag is set if there is at
--       least one aspect for a representation characteristic that has to be
--       delayed and is one of the characteristics that may be inherited by
--       types derived from this type if not overridden. If this flag is set,
--       then types derived from this type have May_Inherit_Delayed_Rep_Aspects
--       set, signalling that Freeze.Inherit_Delayed_Rep_Aspects must be called
--       at the freeze point of the derived type.

--    Has_DIC (synthesized)
--       Defined in all type entities. Set for a private type and its full view
--       when the type is subject to pragma Default_Initial_Condition (DIC), or
--       when the type inherits a DIC pragma from a parent type.

--    Has_Discriminants
--       Defined in all types and subtypes. For types that are allowed to have
--       discriminants (record types and subtypes, task types and subtypes,
--       protected types and subtypes, private types, limited private types,
--       and incomplete types), indicates if the corresponding type or subtype
--       has a known discriminant part. Always false for all other types.

--    Has_Dispatch_Table
--       Defined in E_Record_Types that are tagged. Set to indicate that the
--       corresponding dispatch table is already built. This flag is used to
--       avoid duplicate construction of library level dispatch tables (because
--       the declaration of library level objects cause premature construction
--       of the table); otherwise the code that builds the table is added at
--       the end of the list of declarations of the package.

--    Has_Dynamic_Predicate_Aspect
--       Defined in all types and subtypes. Set if a Dynamic_Predicate aspect
--       was explicitly applied to the type. Generally we treat predicates as
--       static if possible, regardless of whether they are specified using
--       Predicate, Static_Predicate, or Dynamic_Predicate. And if a predicate
--       can be treated as static (i.e. its expression is predicate-static),
--       then the flag Has_Static_Predicate will be set True. But there are
--       cases where legality is affected by the presence of an explicit
--       Dynamic_Predicate aspect. For example, even if a predicate looks
--       static, you can't use it in a case statement if there is an explicit
--       Dynamic_Predicate aspect specified. So test Has_Static_Predicate if
--       you just want to know if the predicate can be evaluated statically,
--       but test Has_Dynamic_Predicate_Aspect to enforce legality rules about
--       the use of dynamic predicates.

--    Has_Entries (synthesized)
--       Applies to concurrent types. True if any entries are declared
--       within the task or protected definition for the type.

--    Has_Enumeration_Rep_Clause
--       Defined in enumeration types. Set if an enumeration representation
--       clause has been given for this enumeration type. Used to prevent more
--       than one enumeration representation clause for a given type. Note
--       that this does not imply a representation with holes, since the rep
--       clause may merely confirm the default 0..N representation.

--    Has_Exit
--       Defined in loop entities. Set if the loop contains an exit statement.

--    Has_Expanded_Contract
--       Defined in functions, procedures, entries, and entry families. Set
--       when a subprogram has a N_Contract node that has been expanded. The
--       flag prevents double expansion of a contract when a construct is
--       rewritten into something else and subsequently reanalyzed/expanded.

--    Has_Foreign_Convention (synthesized)
--       Applies to all entities. Determines if the Convention for the entity
--       is a foreign convention, i.e. non-native: other than Convention_Ada,
--       Convention_Intrinsic, Convention_Entry, Convention_Protected,
--       Convention_Stubbed and Convention_Ada_Pass_By_(Copy,Reference).

--    Has_Forward_Instantiation
--       Defined in package entities. Set for packages that instantiate local
--       generic entities before the corresponding generic body has been seen.
--       If a package has a forward instantiation, we cannot inline subprograms
--       appearing in the same package because the placement requirements of
--       the instance will conflict with the linear elaboration of front-end
--       inlining.

--    Has_Fully_Qualified_Name
--       Defined in all entities. Set if the name in the Chars field has been
--       replaced by the fully qualified name, as used for debug output. See
--       Exp_Dbug for a full description of the use of this flag and also the
--       related flag Has_Qualified_Name.

--    Has_Gigi_Rep_Item
--       Defined in all entities. Set if the rep item chain (referenced by
--       First_Rep_Item and linked through the Next_Rep_Item chain) contains a
--       representation item that needs to be specially processed by the back
--       end, i.e. one of the following items:
--
--          Machine_Attribute pragma
--          Linker_Alias pragma
--          Linker_Constructor pragma
--          Linker_Destructor pragma
--          Weak_External pragma
--          Thread_Local_Storage pragma
--
--       If this flag is set, then the backend should scan the rep item chain
--       to process any of these items that appear. At least one such item will
--       be present.
--
--    Has_Homonym
--       Defined in all entities. Set if an entity has a homonym in the same
--       scope. Used by the backend to generate unique names for all entities.

--    Has_Implicit_Dereference
--       Defined in types and discriminants. Set if the type has an aspect
--       Implicit_Dereference. Set also on the discriminant named in the aspect
--       clause, to simplify type resolution.

--    Has_Independent_Components [implementation base type only]
--       Defined in all types and objects. Set only for a record type or an
--       array type or array object if a valid pragma Independent_Components
--       applies to the type or object. Note that in the case of an object,
--       this flag is only set on the object if there was an explicit pragma
--       for the object. In other words, the proper test for whether an object
--       has independent components is to see if either the object or its base
--       type has this flag set. Note that in the case of a type, the pragma
--       will be chained to the rep item chain of the first subtype in the
--       usual manner. Also set if a pragma Has_Atomic_Components or pragma
--       Has_Aliased_Components applies to the type or object.

--    Has_Inheritable_Invariants [base type only]
--       Defined in all type entities. Set on private types and interface types
--       which define at least one class-wide invariant. Such invariants must
--       be inherited by derived types. The flag is also set on the full view
--       of a private type for completeness.

--    Has_Inherited_DIC [base type only]
--       Defined in all type entities. Set for a derived type which inherits
--       pragma Default_Initial_Condition from a parent type.

--    Has_Inherited_Invariants [base type only]
--       Defined in all type entities. Set on private extensions and derived
--       types which inherit at least one class-wide invariant from a parent or
--       an interface type. The flag is also set on the full view of a private
--       extension for completeness.

--    Has_Initial_Value
--       Defined in entities for variables and out parameters. Set if there
--       is an explicit initial value expression in the declaration of the
--       variable. Note that this is set only if this initial value is
--       explicit, it is not set for the case of implicit initialization
--       of access types or controlled types. Always set to False for out
--       parameters. Also defined in entities for in and in-out parameters,
--       but always false in these cases.

--    Has_Interrupt_Handler (synthesized)
--       Applies to all protected type entities. Set if the protected type
--       definition contains at least one procedure to which a pragma
--       Interrupt_Handler applies.

--    Has_Invariants (synthesized)
--       Defined in all type entities. True if the type defines at least one
--       invariant of its own or inherits at least one class-wide invariant
--       from a parent type or an interface.

--    Has_Limited_View (synth)
--       Defined in all entities. True for non-generic package entities that
--       are non-instances and their Limited_View attribute is present.

--    Has_Loop_Entry_Attributes
--       Defined in E_Loop entities. Set when the loop is subject to at least
--       one attribute 'Loop_Entry. The flag also implies that the loop has
--       already been transformed. See Expand_Loop_Entry_Attribute for details.

--    Has_Machine_Radix_Clause
--       Defined in decimal types and subtypes, set if a Machine_Radix
--       representation clause is present. This flag is used to detect
--       the error of multiple machine radix clauses for a single type.

--    Has_Master_Entity
--       Defined in entities that can appear in the scope stack (see spec
--       of Sem). It is set if a task master entity (_master) has been
--       declared and initialized in the corresponding scope.

--    Has_Missing_Return
--       Defined in functions and generic functions. Set if there is one or
--       more missing return statements in the function. This is used to
--       control wrapping of the body in Exp_Ch6 to ensure that the program
--       error exception is correctly raised in this case at run time.

--    Has_Nested_Block_With_Handler
--       Defined in scope entities. Set if there is a nested block within the
--       scope that has an exception handler and the two scopes are in the
--       same procedure. This is used by the backend for controlling certain
--       optimizations to ensure that they are consistent with exceptions.
--       See documentation in backend for further details.

--    Has_Nested_Subprogram
--       Defined in subprogram entities. Set for a subprogram which contains at
--       least one nested subprogram.

--    Has_Non_Limited_View (synth)
--       Defined in E_Incomplete_Type, E_Incomplete_Subtype, E_Class_Wide_Type,
--       E_Abstract_State entities. True if their Non_Limited_View attribute
--       is present.

--    Has_Non_Null_Abstract_State (synth)
--       Defined in package entities. True if the package is subject to a non-
--       null Abstract_State aspect/pragma.

--    Has_Non_Null_Visible_Refinement (synth)
--       Defined in E_Abstract_State entities. True if the state has a visible
--       refinement of at least one variable or state constituent as expressed
--       in aspect/pragma Refined_State.

--    Has_Non_Standard_Rep [implementation base type only]
--       Defined in all type entities. Set when some representation clause
--       or pragma causes the representation of the item to be significantly
--       modified. In this category are changes of small or radix for a
--       fixed-point type, change of component size for an array, and record
--       or enumeration representation clauses, as well as packed pragmas.
--       All other representation clauses (e.g. Size and Alignment clauses)
--       are not considered to be significant since they do not affect
--       stored bit patterns.

--    Has_Null_Abstract_State (synth)
--       Defined in package entities. True if the package is subject to a null
--       Abstract_State aspect/pragma.

--    Has_Null_Visible_Refinement (synth)
--       Defined in E_Abstract_State entities. True if the state has a visible
--       null refinement as expressed in aspect/pragma Refined_State.

--    Has_Object_Size_Clause
--       Defined in entities for types and subtypes. Set if an Object_Size
--       clause has been processed for the type. Used to prevent multiple
--       Object_Size clauses for a given entity.

--    Has_Out_Or_In_Out_Parameter
--       Present in subprograms, generic subprograms, entries, and entry
--       families. Set if they have at least one OUT or IN OUT parameter
--       (allowed for functions only in Ada 2012).

--    Has_Own_DIC [base type only]
--       Defined in all type entities. Set for a private type and its full view
--       (and its underlying full view, if the full view is itself private)
--       when the type is subject to pragma Default_Initial_Condition.

--    Has_Own_Invariants [base type only]
--       Defined in all type entities. Set on any type that defines at least
--       one invariant of its own.

--       Note: this flag is set on both partial and full view of types to which
--       an Invariant pragma or aspect applies, and on the underlying full view
--       if the full view is private.

--    Has_Partial_Visible_Refinement
--       Defined in E_Abstract_State entities. Set when a state has at least
--       one refinement constituent subject to indicator Part_Of, and analysis
--       is in the region between the declaration of the first constituent for
--       this abstract state (in the private part of the package) and the end
--       of the package spec or body with visibility over this private part
--       (which includes the package itself and its child packages).

--    Has_Per_Object_Constraint
--       Defined in E_Component entities. Set if the subtype of the component
--       has a per object constraint. Per object constraints result from the
--       following situations:
--
--       1. N_Attribute_Reference - when the prefix is the enclosing type and
--          the attribute is Access.
--       2. N_Discriminant_Association - when the expression uses the
--          discriminant of the enclosing type.
--       3. N_Index_Or_Discriminant_Constraint - when at least one of the
--          individual constraints is a per object constraint.
--       4. N_Range - when the lower or upper bound uses the discriminant of
--          the enclosing type.
--       5. N_Range_Constraint - when the range expression uses the
--          discriminant of the enclosing type.

--    Has_Pragma_Controlled [implementation base type only]
--       Defined in access type entities. It is set if a pragma Controlled
--       applies to the access type.

--    Has_Pragma_Elaborate_Body
--       Defined in all entities. Set in compilation unit entities if a
--       pragma Elaborate_Body applies to the compilation unit.

--    Has_Pragma_Inline
--       Defined in all entities. Set for functions and procedures for which a
--       pragma Inline or Inline_Always applies to the subprogram. Note that
--       this flag can be set even if Is_Inlined is not set. This happens for
--       pragma Inline (if Inline_Active is False). In other words, the flag
--       Has_Pragma_Inline represents the formal semantic status, and is used
--       for checking semantic correctness. The flag Is_Inlined indicates
--       whether inlining is actually active for the entity.

--    Has_Pragma_Inline_Always
--       Defined in all entities. Set for functions and procedures for which a
--       pragma Inline_Always applies. Note that if this flag is set, the flag
--       Has_Pragma_Inline is also set.

--    Has_Pragma_No_Inline
--       Defined in all entities. Set for functions and procedures for which a
--       pragma No_Inline applies. Note that if this flag is set, the flag
--       Has_Pragma_Inline_Always cannot be set.

--    Has_Pragma_Ordered [implementation base type only]
--       Defined in entities for enumeration types. If set indicates that a
--       valid pragma Ordered was given for the type. This flag is inherited
--       by derived enumeration types. We don't need to distinguish the derived
--       case since we allow multiple occurrences of this pragma anyway.

--    Has_Pragma_Pack [implementation base type only]
--       Defined in array and record type entities. If set, indicates that a
--       valid pragma Pack was given for the type. Note that this flag is not
--       inherited by derived type. See also the Is_Packed flag.

--    Has_Pragma_Preelab_Init
--       Defined in type and subtype entities. If set indicates that a valid
--       pragma Preelaborable_Initialization applies to the type.

--    Has_Pragma_Pure
--       Defined in all entities. If set, indicates that a valid pragma Pure
--       was given for the entity. In some cases, we need to test whether
--       Is_Pure was explicitly set using this pragma.

--    Has_Pragma_Pure_Function
--       Defined in all entities. If set, indicates that a valid pragma
--       Pure_Function was given for the entity. In some cases, we need to test
--       whether Is_Pure was explicitly set using this pragma. We also set
--       this flag for some internal entities that we know should be treated
--       as pure for optimization purposes.

--    Has_Pragma_Thread_Local_Storage
--       Defined in all entities. If set, indicates that a valid pragma
--       Thread_Local_Storage was given for the entity.

--    Has_Pragma_Unmodified
--       Defined in all entities. Can only be set for variables (E_Variable,
--       E_Out_Parameter, E_In_Out_Parameter). Set if a valid pragma Unmodified
--       applies to the variable, indicating that no warning should be given
--       if the entity is never modified. Note that clients should generally
--       not test this flag directly, but instead use function Has_Unmodified.

--    Has_Pragma_Unreferenced
--       Defined in all entities. Set if a valid pragma Unreferenced applies
--       to the entity, indicating that no warning should be given if the
--       entity has no references, but a warning should be given if it is
--       in fact referenced. For private types, this flag is set in both the
--       private entity and full entity if the pragma applies to either. Note
--       that clients should generally not test this flag directly, but instead
--       use function Has_Unreferenced.

--    Has_Pragma_Unreferenced_Objects
--       Defined in all entities. Set if a valid pragma Unused applies to an
--       entity, indicating that warnings should be given if the entity is
--       modified or referenced. This pragma is equivalent to a pair of
--       Unmodified and Unreferenced pragmas.

--    Has_Pragma_Unused
--       Defined in all entities. Set if a valid pragma Unused applies to a
--       variable or entity, indicating that warnings should not be given if
--       it is never modified or referenced. Note: This pragma is exactly
--       equivalent Unmodified and Unreference combined.

--    Has_Predicates
--       Defined in type and subtype entities. Set if a pragma Predicate or
--       Predicate aspect applies to the type or subtype, or if it inherits a
--       Predicate aspect from its parent or progenitor types.
--
--       Note: this flag is set on both partial and full view of types to which
--       a Predicate pragma or aspect applies, and on the underlying full view
--       if the full view is private.

--    Has_Primitive_Operations [base type only]
--       Defined in all type entities. Set if at least one primitive operation
--       is defined for the type.

--    Has_Private_Ancestor
--       Applies to type extensions. True if some ancestor is derived from a
--       private type, making some components invisible and aggregates illegal.
--       This flag is set at the point of derivation. The legality of the
--       aggregate must be rechecked because it also depends on the visibility
--       at the point the aggregate is resolved. See sem_aggr.adb. This is part
--       of AI05-0115.

--    Has_Private_Declaration
--       Defined in all entities. Set if it is the defining entity of a private
--       type declaration or its corresponding full declaration. This flag is
--       thus preserved when the full and the partial views are exchanged, to
--       indicate if a full type declaration is a completion. Used for semantic
--       checks in E.4(18) and elsewhere.

--    Has_Private_Extension
--       Defined in tagged types. Set to indicate that the tagged type has some
--       private extension. Used to report a warning on public primitives added
--       after defining its private extensions.

--    Has_Protected [base type only]
--       Defined in all type entities. Set on protected types themselves, and
--       also (recursively) on any composite type which has a component for
--       which Has_Protected is set, unless the protected type is declared in
--       the private part of an internal unit. The meaning is that restrictions
--       for protected types apply to this type. Note: the flag is not set on
--       access types, even if they designate an object that Has_Protected.

--    Has_Qualified_Name
--       Defined in all entities. Set if the name in the Chars field has
--       been replaced by its qualified name, as used for debug output. See
--       Exp_Dbug for a full description of qualification requirements. For
--       some entities, the name is the fully qualified name, but there are
--       exceptions. In particular, for local variables in procedures, we
--       do not include the procedure itself or higher scopes. See also the
--       flag Has_Fully_Qualified_Name, which is set if the name does indeed
--       include the fully qualified name.

--    Has_RACW
--       Defined in package spec entities. Set if the spec contains the
--       declaration of a remote access-to-classwide type.

--    Has_Record_Rep_Clause [implementation base type only]
--       Defined in record types. Set if a record representation clause has
--       been given for this record type. Used to prevent more than one such
--       clause for a given record type. Note that this is initially cleared
--       for a derived type, even though the representation is inherited. See
--       also the flag Has_Specified_Layout.

--    Has_Recursive_Call
--       Defined in procedures. Set if a direct parameterless recursive call
--       is detected while analyzing the body. Used to activate some error
--       checks for infinite recursion.

--    Has_Shift_Operator [base type only]
--       Defined in integer types. Set in the base type of an integer type for
--       which at least one of the shift operators is defined.

--    Has_Size_Clause
--       Defined in entities for types and objects. Set if a size or value size
--       clause is defined for the entity. Used to prevent multiple clauses
--       for a given entity. Note that it is always initially cleared for a
--       derived type, even though the Size or Value_Size clause for such a
--       type might be inherited from an ancestor type.

--    Has_Small_Clause
--       Defined in ordinary fixed point types (but not subtypes). Indicates
--       that a small clause has been given for the entity. Used to prevent
--       multiple Small clauses for a given entity. Note that it is always
--       initially cleared for a derived type, even though the Small for such
--       a type is inherited from a Small clause given for the parent type.

--    Has_Specified_Layout [implementation base type only]
--       Defined in all type entities. Set for a record type or subtype if
--       the record layout has been specified by a record representation
--       clause. Note that this differs from the flag Has_Record_Rep_Clause
--       in that it is inherited by a derived type. Has_Record_Rep_Clause is
--       used to indicate that the type is mentioned explicitly in a record
--       representation clause, and thus is not inherited by a derived type.
--       This flag is always False for non-record types.

--    Has_Specified_Stream_Input
--    Has_Specified_Stream_Output
--    Has_Specified_Stream_Read
--    Has_Specified_Stream_Write
--       Defined in all type and subtype entities. Set for a given view if the
--       corresponding stream-oriented attribute has been defined by an
--       attribute definition clause. When such a clause occurs, a TSS is set
--       on the underlying full view; the flags are used to track visibility of
--       the attribute definition clause for partial or incomplete views.

--    Has_Static_Discriminants
--       Defined in record subtypes constrained by discriminant values. Set if
--       all the discriminant values have static values, meaning that in the
--       case of a variant record, the component list can be trimmed down to
--       include only the components corresponding to these discriminants.

--    Has_Static_Predicate
--       Defined in all types and subtypes. Set if the type (which must be a
--       scalar type) has a predicate whose expression is predicate-static.
--       This can result from the use of any Predicate, Static_Predicate, or
--       Dynamic_Predicate aspect. We can distinguish these cases by testing
--       Has_Static_Predicate_Aspect and Has_Dynamic_Predicate_Aspect. See
--       description of the latter flag for further information on dynamic
--       predicates which are also static.

--    Has_Static_Predicate_Aspect
--       Defined in all types and subtypes. Set if a Static_Predicate aspect
--       applies to the type. Note that we can tell if a static predicate is
--       present by looking at Has_Static_Predicate, but this could have come
--       from a Predicate aspect or pragma or even from a Dynamic_Predicate
--       aspect. When we need to know the difference (e.g. to know what set of
--       check policies apply, use this flag and Has_Dynamic_Predicate_Aspect
--       to determine which case we have).

--    Has_Storage_Size_Clause [implementation base type only]
--       Defined in task types and access types. It is set if a Storage_Size
--       clause is present for the type. Used to prevent multiple clauses for
--       one type. Note that this flag is initially cleared for a derived type
--       even though the Storage_Size for such a type is inherited from a
--       Storage_Size clause given for the parent type. Note that in the case
--       of access types, this flag is defined only in the root type, since a
--       storage size clause cannot be given to a derived type.

--    Has_Stream_Size_Clause
--       Defined in all entities. It is set for types which have a Stream_Size
--       clause attribute. Used to prevent multiple Stream_Size clauses for a
--       given entity, and also whether it is necessary to check for a stream
--       size clause.

--    Has_Task [base type only]
--       Defined in all type entities. Set on task types themselves, and also
--       (recursively) on any composite type which has a component for which
--       Has_Task is set. The meaning is that an allocator or declaration of
--       such an object must create the required tasks. Note: the flag is not
--       set on access types, even if they designate an object that Has_Task.

--    Has_Timing_Event [base type only]
--       Defined in all type entities. Set on language defined type
--       Ada.Real_Time.Timing_Events.Timing_Event, and also (recursively) on
--       any composite type which has a component for which Has_Timing_Event
--       is set. Used for the No_Local_Timing_Event restriction.

--    Has_Thunks
--       Applies to E_Constant entities marked Is_Tag. True for secondary tag
--       referencing a dispatch table whose contents are pointers to thunks.

--    Has_Unchecked_Union [base type only]
--       Defined in all type entities. Set on unchecked unions themselves
--       and (recursively) on any composite type which has a component for
--       which Has_Unchecked_Union is set. The meaning is that a comparison
--       operation or 'Valid_Scalars reference for the type is not permitted.
--       Note that the flag is not set on access types, even if they designate
--       an object that has the flag Has_Unchecked_Union set.

--    Has_Unknown_Discriminants
--       Defined in all entities. Set for types with unknown discriminants.
--       Types can have unknown discriminants either from their declaration or
--       through type derivation. The use of this flag exactly meets the spec
--       in RM 3.7(26). Note that all class-wide types are considered to have
--       unknown discriminants. Note that both flags Has_Discriminants and
--       Has_Unknown_Discriminants may be true for a type. Class-wide types and
--       their subtypes have unknown discriminants and can have declared ones
--       as well. Private types declared with unknown discriminants may have a
--       full view that has explicit discriminants, and both flag will be set
--       on the partial view, to ensure that discriminants are properly
--       inherited in certain contexts.

--    Has_Visible_Refinement
--       Defined in E_Abstract_State entities. Set when a state has at least
--       one refinement constituent and analysis is in the region between
--       pragma Refined_State and the end of the package body declarations.

--    Has_Volatile_Components [implementation base type only]
--       Defined in all types and objects. Set only for an array type or array
--       object if a valid pragma Volatile_Components or a valid pragma
--       Atomic_Components applies to the type or object. Note that in the case
--       of an object, this flag is only set on the object if there was an
--       explicit pragma for the object. In other words, the proper test for
--       whether an object has volatile components is to see if either the
--       object or its base type has this flag set. Note that in the case of a
--       type the pragma will be chained to the rep item chain of the first
--       subtype in the usual manner.

--    Has_Xref_Entry
--       Defined in all entities. Set if an entity has an entry in the Xref
--       information generated in ali files. This is true for all source
--       entities in the extended main source file. It is also true of entities
--       in other packages that are referenced directly or indirectly from the
--       main source file (indirect reference occurs when the main source file
--       references an entity with a type reference. See package Lib.Xref for
--       further details).

--    Has_Yield_Aspect
--       Defined in subprograms, generic subprograms, entries, entry families.
--       Set if the entity has aspect Yield.

--    Hiding_Loop_Variable
--       Defined in variables. Set only if a variable of a discrete type is
--       hidden by a loop variable in the same local scope, in which case
--       the Hiding_Loop_Variable field of the hidden variable points to
--       the E_Loop_Parameter entity doing the hiding. Used in processing
--       warning messages if the hidden variable turns out to be unused
--       or is referenced without being set.

--    Hidden_In_Formal_Instance
--       Defined on actuals for formal packages. Entities on the list are
--       formals that are hidden outside of the formal package when this
--       package is not declared with a box, or the formal itself is not
--       defaulted (see RM 12.7 (10)). Their visibility is restored on exit
--       from the current generic, because the actual for the formal package
--       may be used subsequently in the current unit.

--    Homonym
--       Defined in all entities. Link for list of entities that have the
--       same source name and that are declared in the same or enclosing
--       scopes. Homonyms in the same scope are overloaded. Used for name
--       resolution and for the generation of debugging information.

--    Ignore_SPARK_Mode_Pragmas
--       Present in concurrent type, entry, operator, [generic] package,
--       package body, [generic] subprogram, and subprogram body entities.
--       Set when the entity appears in an instance subject to SPARK_Mode
--       "off" and indicates that all SPARK_Mode pragmas found within must
--       be ignored.

--    Ignored_Class_Postconditions
--       Defined on subprogram entities. Set if the subprogram has class-wide
--       postconditions. Denotes the (and-then) expression built by merging
--       inherited ignored class-wide postconditions with its own ignored
--       class-wide postconditions.

--    Ignored_Class_Preconditions
--       Defined on subprogram entities. Set if the subprogram has class-wide
--       preconditions. Denotes the (or-else) expression built by merging
--       inherited ignored class-wide preconditions with its own ignored
--       class-wide preconditions.

--    Implementation_Base_Type (synthesized)
--       Applies to all entities. For types, similar to Base_Type, but never
--       returns a private type when applied to a non-private type. Instead in
--       this case, it always returns the Underlying_Type of the base type, so
--       that we still have a concrete type. For entities other than types,
--       returns the entity unchanged.

--    Import_Pragma
--       Defined in subprogram entities. Set if a valid pragma Import or pragma
--       Import_Function or pragma Import_Procedure applies to the subprogram,
--       in which case this field points to the pragma (we can't use the normal
--       Rep_Item chain mechanism, because a single pragma Import can apply
--       to multiple subprogram entities).

--    In_Package_Body
--       Defined in package entities. Set on the entity that denotes the
--       package (the defining occurrence of the package declaration) while
--       analyzing and expanding the package body. Reset on completion of
--       analysis/expansion.

--    In_Private_Part
--       Defined in all entities. Can be set only in package entities and
--       objects. For package entities, this flag is set to indicate that the
--       private part of the package is being analyzed. The flag is reset at
--       the end of the package declaration. For objects it indicates that the
--       declaration of the object occurs in the private part of a package.

--    Incomplete_Actuals
--       Defined on package entities that are instances. Indicates the actuals
--       types in the instantiation that are limited views. If this list is
--       not empty, the instantiation, which appears in a package declaration,
--       is relocated to the corresponding package body, which must have a
--       corresponding nonlimited with_clause.

--    Indirect_Call_Wrapper
--       Defined on subprogram entities. Set if the subprogram has class-wide
--       preconditions. Denotes the internal wrapper that checks preconditions
--       and invokes the subprogram body. Subp'Access points to the indirect
--       call wrapper if available.

--    Initialization_Statements
--       Defined in constants and variables. For a composite object initialized
--       with an aggregate that has been converted to a sequence of
--       assignments, points to a compound statement containing the
--       assignments.

--    Inner_Instances
--       Defined in generic units. Contains element list of units that are
--       instantiated within the given generic. Used to diagnose circular
--       instantiations.

--    Interface_Alias
--       Defined in subprograms that cover a primitive operation of an abstract
--       interface type. Can be set only if the Is_Hidden flag is also set,
--       since such entities are always hidden. Points to its associated
--       interface subprogram. It is used to register the subprogram in
--       secondary dispatch table of the interface (Ada 2005: AI-251).

--    Interface_Name
--       Defined in constants, variables, exceptions, functions, procedures,
--       and packages. Set to Empty unless an export, import, or interface name
--       pragma has explicitly specified an external name, in which case it
--       references an N_String_Literal node for the specified external name.
--       Note that if this field is Empty, and Is_Imported or Is_Exported is
--       set, then the default interface name is the name of the entity, cased
--       in a manner that is appropriate to the system in use. Note that
--       Interface_Name is ignored if an address clause is present (since it
--       is meaningless in this case).

--    Interfaces
--       Defined in record types and subtypes. List of abstract interfaces
--       implemented by a tagged type that are not already implemented by the
--       ancestors (Ada 2005: AI-251).

--    Invariant_Procedure (synthesized)
--       Defined in types and subtypes. Set for private types and their full
--       views if one or more [class-wide] invariants apply to the type, or
--       when the type inherits class-wide invariants from a parent type or
--       an interface, or when the type is an array and its component type is
--       subject to an invariant, or when the type is record and contains a
--       component subject to an invariant (property is recursive). Points to
--       to the entity for a procedure which checks all these invariants. The
--       invariant procedure takes a single argument of the given type, and
--       returns if the invariant holds, or raises exception Assertion_Error
--       with an appropriate message if it does not hold. This attribute is
--       defined but always Empty for private subtypes.

--       Note: the reason this is marked as a synthesized attribute is that the
--       way this is stored is as an element of the Subprograms_For_Type field.

--    In_Use
--       Defined in packages and types. Set when analyzing a use clause for
--       the corresponding entity. Reset at end of corresponding declarative
--       part. The flag on a type is also used to determine the visibility of
--       the primitive operators of the type.
--
--       Note that manipulation of scopes on the scope stack will also cause
--       the flag to be set/unset since the setting of scopes affects
--       visibility.

--    Is_Abstract_Subprogram
--       Defined in all subprograms and entries. Set for abstract subprograms.
--       Always False for enumeration literals and entries. See also
--       Requires_Overriding.

--    Is_Abstract_Type
--       Defined in all types. Set for abstract types.

--    Is_Access_Constant
--       Defined in access types and subtypes. Indicates that the keyword
--       constant was present in the access type definition.

--    Is_Access_Protected_Subprogram_Type (synthesized)
--       Applies to all types, true for named and anonymous access to
--       protected subprograms.

--    Is_Access_Type (synthesized)
--       Applies to all entities, true for access types and subtypes

--    Is_Access_Object_Type (synthesized)
--       Applies to all entities, true for access-to-object types and subtypes

--    Is_Activation_Record
--       Applies to E_In_Parameters generated in Exp_Unst for nested
--       subprograms, to mark the added formal that carries the activation
--       record created in the enclosing subprogram.

--    Is_Actual_Subtype
--       Defined on all types, true for the generated constrained subtypes
--       that are built for unconstrained composite actuals.

--    Is_Ada_2005_Only
--       Defined in all entities, true if a valid pragma Ada_05 or Ada_2005
--       applies to the entity which specifically names the entity, indicating
--       that the entity is Ada 2005 only. Note that this flag is not set if
--       the entity is part of a unit compiled with the normal no-argument form
--       of pragma Ada_05 or Ada_2005.

--    Is_Ada_2012_Only
--       Defined in all entities, true if a valid pragma Ada_12 or Ada_2012
--       applies to the entity which specifically names the entity, indicating
--       that the entity is Ada 2012 only. Note that this flag is not set if
--       the entity is part of a unit compiled with the normal no-argument form
--       of pragma Ada_12 or Ada_2012.

--    Is_Ada_2022_Only
--       Defined in all entities, true if a valid pragma Ada_2022 applies to
--       the entity which specifically names the entity, indicating that the
--       entity is Ada 2022 only. Note that this flag is not set if the entity
--       is part of a unit compiled with the normal no-argument form of pragma
--       Ada_2022.

--    Is_Aliased
--       Defined in all entities. Set for objects and types whose declarations
--       carry the keyword aliased, and on record components that have the
--       keyword. For Ada 2012, also applies to formal parameters.

--    Is_Array_Type (synthesized)
--       Applies to all entities, true for array types and subtypes

--    Is_Asynchronous
--       Defined in all type entities and in procedure entities. Set
--       if a pragma Asynchronous applies to the entity.

--    Is_Atomic
--       Defined in all type entities, and also in constants, components, and
--       variables. Set if a pragma Atomic or Shared applies to the entity.
--       In the case of private and incomplete types, this flag is set in
--       both the partial view and the full view.

--    Is_Full_Access (synth)
--       Defined in all type entities, and also in constants, components and
--       variables. Set if an aspect/pragma Atomic/Shared, or an aspect/pragma
--       Volatile_Full_Access or an Ada 2022 aspect Full_Access_Only applies
--       to the entity. In the case of private and incomplete types, the flag
--       applies to both the partial view and the full view.

--    Is_Base_Type (synthesized)
--       Applies to type and subtype entities. True if entity is a base type.

--    Is_Bit_Packed_Array [implementation base type only]
--       Defined in all entities. This flag is set for a packed array type that
--       is bit-packed (i.e. the component size is known by the front end and
--       is in the range 1-63 but not a multiple of 8). Is_Packed is always set
--       if Is_Bit_Packed_Array is set, but it is possible for Is_Packed to be
--       set without Is_Bit_Packed_Array if the component size is not known by
--       the front-end or for the case of an array having one or more index
--       types that are enumeration types with non-standard representation.

--    Is_Boolean_Type (synthesized)
--       Applies to all entities, true for boolean types and subtypes,
--       i.e. Standard.Boolean and all types ultimately derived from it.

--    Is_Called
--       Defined in subprograms and packages. Set if a subprogram is called
--       from the unit being compiled or a unit in the closure. Also set for
--       a package that contains called subprograms. Used only for inlining.

--    Is_Character_Type
--       Defined in all entities. Set for character types and subtypes,
--       i.e. enumeration types that have at least one character literal.

--    Is_Checked_Ghost_Entity
--       Applies to all entities. Set for abstract states, [generic] packages,
--       [generic] subprograms, components, discriminants, formal parameters,
--       objects, package bodies, subprogram bodies, and [sub]types subject to
--       pragma Ghost or inherit "ghostness" from an enclosing construct, and
--       subject to Assertion_Policy Ghost => Check.

--    Is_Child_Unit
--       Defined in all entities. Set only for defining entities of program
--       units that are child units (but False for subunits).

--    Is_Class_Wide_Equivalent_Type
--       Defined in record types and subtypes. Set to True, if the type acts
--       as a class-wide equivalent type, i.e. the Equivalent_Type field of
--       some class-wide subtype entity references this record type.

--    Is_Class_Wide_Type (synthesized)
--       Applies to all entities, true for class wide types and subtypes

--    Is_Class_Wide_Wrapper
--       Defined in subprogram entities. Indicates that it has been created as
--       a wrapper in a generic/instance scenario involving a formal type and
--       a generic primitive operation when the actual is a class-wide type.

--    Is_Compilation_Unit
--       Defined in all entities. Set if the entity is a package or subprogram
--       entity for a compilation unit other than a subunit (since we treat
--       subunits as part of the same compilation operation as the ultimate
--       parent, we do not consider them to be separate units for this flag).

--    Is_Completely_Hidden
--       Defined on discriminants. Only set on stored discriminants of
--       untagged types. When set, the entity is a stored discriminant of a
--       derived untagged type which is not directly visible in the derived
--       type because the derived type or one of its ancestors have renamed the
--       discriminants in the root type. Note: there are stored discriminants
--       which are not Completely_Hidden (e.g. discriminants of a root type).

--    Is_Composite_Type (synthesized)
--       Applies to all entities, true for all composite types and subtypes.
--       Either Is_Composite_Type or Is_Elementary_Type (but not both) is true
--       of any type.

--    Is_Concurrent_Record_Type
--       Defined in record types and subtypes. Set if the type was created
--       by the expander to represent a task or protected type. For every
--       concurrent type, such as record type is constructed, and task and
--       protected objects are instances of this record type at run time
--       (The backend will replace declarations of the concurrent type using
--       the declarations of the corresponding record type). See Exp_Ch9 for
--       further details.

--    Is_Concurrent_Type (synthesized)
--       Applies to all entities, true for task types and subtypes and for
--       protected types and subtypes.

--    Is_Constant_Object (synthesized)
--       Applies to all entities, true for E_Constant, E_Loop_Parameter, and
--       E_In_Parameter entities.

--    Is_Constrained
--       Defined in types or subtypes which may have index, discriminant
--       or range constraint (i.e. array types and subtypes, record types
--       and subtypes, string types and subtypes, and all numeric types).
--       Set if the type or subtype is constrained.

--    Is_Constr_Subt_For_U_Nominal
--       Defined in all types and subtypes. Set only for the constructed
--       subtype of an object whose nominal subtype is unconstrained. Note
--       that the constructed subtype itself will be constrained.

--    Is_Constr_Subt_For_UN_Aliased
--       Defined in all types and subtypes. This flag can be set only if
--       Is_Constr_Subt_For_U_Nominal is also set. It indicates that in
--       addition the object concerned is aliased. This flag is used by
--       the backend to determine whether a template must be constructed.

--    Is_Constructor
--       Defined in function and procedure entities. Set if a pragma
--       CPP_Constructor applies to the subprogram.

--    Is_Controlled_Active [base type only]
--       Defined in all type entities. Indicates that the type is controlled,
--       i.e. is either a descendant of Ada.Finalization.Controlled or of
--       Ada.Finalization.Limited_Controlled.

--    Is_Controlled (synth) [base type only]
--       Defined in all type entities. Set if Is_Controlled_Active is set for
--       the type, and Disable_Controlled is not set.

--    Is_Controlling_Formal
--       Defined in all Formal_Kind entities. Marks the controlling parameters
--       of dispatching operations.

--    Is_CPP_Class
--       Defined in all type entities, set only for tagged types to which a
--       valid pragma Import (CPP, ...) or pragma CPP_Class has been applied.

--    Is_CUDA_Kernel
--       Defined in function and procedure entities. Set if the subprogram is a
--       CUDA kernel.

--    Is_Decimal_Fixed_Point_Type (synthesized)
--       Applies to all type entities, true for decimal fixed point
--       types and subtypes.

--    Is_Descendant_Of_Address
--       Defined in all entities. True if the entity is type System.Address,
--       or (recursively) a subtype or derived type of System.Address.

--    Is_DIC_Procedure
--       Defined in functions and procedures. Set for a generated procedure
--       which verifies the assumption of pragma Default_Initial_Condition at
--       run time.

--    Is_Discrete_Or_Fixed_Point_Type (synthesized)
--       Applies to all entities, true for all discrete types and subtypes
--       and all fixed-point types and subtypes.

--    Is_Discrete_Type (synthesized)
--       Applies to all entities, true for all discrete types and subtypes

--    Is_Discrim_SO_Function
--       Defined in all entities. Set only in E_Function entities that Layout
--       creates to compute discriminant-dependent dynamic size/offset values.

--    Is_Discriminant_Check_Function
--       Defined in all entities. Set only in E_Function entities for functions
--       created to do discriminant checks.

--    Is_Discriminal (synthesized)
--       Applies to all entities, true for renamings of discriminants. Such
--       entities appear as constants or IN parameters.

--    Is_Dispatch_Table_Entity
--       Applies to all entities. Set to indicate to the backend that this
--       entity is associated with a dispatch table.

--    Is_Dispatch_Table_Wrapper
--       Applies to all entities. Set on wrappers built when the subprogram has
--       class-wide preconditions or class-wide postconditions affected by
--       overriding (AI12-0195).

--    Is_Dispatching_Operation
--       Defined in all entities. Set for procedures, functions, generic
--       procedures, and generic functions if the corresponding operation
--       is dispatching.

--    Is_Dynamic_Scope (synthesized)
--       Applies to all Entities. Returns True if the entity is a dynamic
--       scope (i.e. a block, subprogram, task_type, entry or extended return
--       statement).

--    Is_Elaboration_Checks_OK_Id
--       Defined in elaboration targets (see terminology in Sem_Elab). Set when
--       the target appears in a region which is subject to enabled elaboration
--       checks. Such targets are allowed to generate run-time conditional ABE
--       checks or guaranteed ABE failures.

--    Is_Elaboration_Target (synthesized)
--       Applies to all entities, True only for elaboration targets (see the
--       terminology in Sem_Elab).

--    Is_Elaboration_Warnings_OK_Id
--       Defined in elaboration targets (see terminology in Sem_Elab). Set when
--       the target appears in a region with elaboration warnings enabled.

--    Is_Elementary_Type (synthesized)
--       Applies to all entities, True for all elementary types and subtypes.
--       Either Is_Composite_Type or Is_Elementary_Type (but not both) is true
--       of any type.

--    Is_Eliminated
--       Defined in type entities, subprogram entities, and object entities.
--       Indicates that the corresponding entity has been eliminated by use
--       of pragma Eliminate. Also used to mark subprogram entities whose
--       declaration and body are within unreachable code that is removed.

--    Is_Entry (synthesized)
--       Applies to all entities, True only for entry and entry family
--       entities and False for all other entity kinds.

--    Is_Entry_Formal
--       Defined in all entities. Set only for entry formals (which can only
--       be in, in-out or out parameters). This flag is used to speed up the
--       test for the need to replace references in Exp_Ch2.

--    Is_Entry_Wrapper
--       Defined on wrappers created for entries that have precondition or
--       postcondition aspects.

--    Is_Enumeration_Type (synthesized)
--       Defined in all entities, true for enumeration types and subtypes

--    Is_Exception_Handler
--       Defined in blocks. Set if the block serves only as a scope of an
--       exception handler with a choice parameter. Such a block does not
--       physically appear in the tree.

--    Is_Exported
--       Defined in all entities. Set if the entity is exported. For now we
--       only allow the export of constants, exceptions, functions, procedures
--       and variables, but that may well change later on. Exceptions can only
--       be exported in the Java VM implementation of GNAT, which is retired.

--    Is_External_State (synthesized)
--       Applies to all entities, true for abstract states that are subject to
--       option External or Synchronous.

--    Is_Finalized_Transient
--       Defined in constants, loop parameters of generalized iterators, and
--       variables. Set when a transient object has been finalized by one of
--       the transient finalization mechanisms. The flag prevents the double
--       finalization of the object.

--    Is_Finalizer (synthesized)
--       Applies to all entities, true for procedures containing finalization
--       code to process local or library level objects.

--    Is_First_Subtype
--       Defined in all entities. True for first subtypes (RM 3.2.1(6)),
--       i.e. the entity in the type declaration that introduced the type.
--       This may be the base type itself (e.g. for record declarations and
--       enumeration type declarations), or it may be the first subtype of
--       an anonymous base type (e.g. for integer type declarations or
--       constrained array declarations).

--    Is_Fixed_Lower_Bound_Array_Subtype
--       Defined in type entities. True for unconstrained array types and
--       subtypes where at least one index has a range specified with a fixed
--       lower bound (range syntax is "<expression> .. <>").

--    Is_Fixed_Lower_Bound_Index_Subtype
--       Defined in type entities. True for an index of an unconstrained array
--       type or subtype whose range is specified with a fixed lower bound
--       (range syntax is "<expression> .. <>").

--    Is_Fixed_Point_Type (synthesized)
--       Applies to all entities, true for decimal and ordinary fixed
--       point types and subtypes.

--    Is_Floating_Point_Type (synthesized)
--       Applies to all entities, true for float types and subtypes

--    Is_Formal (synthesized)
--       Applies to all entities, true for IN, IN OUT and OUT parameters

--    Is_Formal_Object (synthesized)
--       Applies to all entities, true for generic IN and IN OUT parameters

--    Is_Formal_Subprogram
--       Defined in all entities. Set for generic formal subprograms.

--    Is_Frozen
--       Defined in all type and subtype entities. Set if type or subtype has
--       been frozen.

--    Is_Generic_Actual_Subprogram
--       Defined on functions and procedures. Set on the entity of the renaming
--       declaration created within an instance for an actual subprogram.
--       Used to generate constraint checks on calls to these subprograms, even
--       within an instance of a predefined run-time unit, in which checks
--       are otherwise suppressed.

--    Is_Generic_Actual_Type
--       Defined in all type and subtype entities. Set in the subtype
--       declaration that renames the generic formal as a subtype of the
--       actual. Guarantees that the subtype is not static within the instance.
--       Also used during analysis of an instance, to simplify resolution of
--       accidental overloading that occurs when different formal types get the
--       same actual.

--    Is_Generic_Instance
--       Defined in all entities. Set to indicate that the entity is an
--       instance of a generic unit, or a formal package (which is an instance
--       of the template).

--    Is_Generic_Subprogram (synthesized)
--       Applies to all entities. Yields True for a generic subprogram
--       (generic function, generic subprogram), False for all other entities.

--    Is_Generic_Type
--       Defined in all entities. Set for types which are generic formal types.
--       Such types have an Ekind that corresponds to their classification, so
--       the Ekind cannot be used to identify generic formal types.

--    Is_Generic_Unit (synthesized)
--       Applies to all entities. Yields True for a generic unit (generic
--       package, generic function, generic procedure), and False for all
--       other entities.

--    Is_Ghost_Entity (synthesized)
--       Applies to all entities. Yields True for abstract states, [generic]
--       packages, [generic] subprograms, components, discriminants, formal
--       parameters, objects, package bodies, subprogram bodies, and [sub]types
--       subject to pragma Ghost or those that inherit the Ghost property from
--       an enclosing construct.

--    Is_Hidden
--       Defined in all entities. Set for all entities declared in the
--       private part or body of a package. Also marks generic formals of a
--       formal package declared without a box. For library level entities,
--       this flag is set if the entity is not publicly visible. This flag
--       is reset when compiling the body of the package where the entity
--       is declared, when compiling the private part or body of a public
--       child unit, and when compiling a private child unit (see Install_
--       Private_Declaration in sem_ch7).

--    Is_Hidden_Non_Overridden_Subpgm
--       Defined in all entities. Set for implicitly declared subprograms
--       that require overriding or are null procedures, and are hidden by
--       a non-fully conformant homograph with the same characteristics
--       (Ada RM 8.3 12.3/2).

--    Is_Hidden_Open_Scope
--       Defined in all entities. Set for a scope that contains the
--       instantiation of a child unit, and whose entities are not visible
--       during analysis of the instance.

--    Is_Ignored_Ghost_Entity
--       Applies to all entities. Set for abstract states, [generic] packages,
--       [generic] subprograms, components, discriminants, formal parameters,
--       objects, package bodies, subprogram bodies, and [sub]types subject to
--       pragma Ghost or inherit "ghostness" from an enclosing construct, and
--       subject to Assertion_Policy Ghost => Ignore.

--    Is_Ignored_Transient
--       Defined in constants, loop parameters of generalized iterators, and
--       variables. Set when a transient object must be processed by one of
--       the transient finalization mechanisms. Once marked, a transient is
--       intentionally ignored by the general finalization mechanism because
--       its clean up actions are context specific.

--    Is_Immediately_Visible
--       Defined in all entities. Set if entity is immediately visible, i.e.
--       is defined in some currently open scope (RM 8.3(4)).

--    Is_Implementation_Defined
--       Defined in all entities. Set if a pragma Implementation_Defined is
--       applied to the pragma. Used to mark all implementation defined
--       identifiers in standard library packages, and to implement the
--       restriction No_Implementation_Identifiers.

--    Is_Imported
--       Defined in all entities. Set if the entity is imported. For now we
--       only allow the import of exceptions, functions, procedures, packages,
--       constants, and variables. Exceptions, packages, and types can only be
--       imported in the Java VM implementation, which is retired.

--    Is_Incomplete_Or_Private_Type (synthesized)
--       Applies to all entities, true for private and incomplete types

--    Is_Incomplete_Type (synthesized)
--       Applies to all entities, true for incomplete types and subtypes

--    Is_Independent
--       Defined in all types and objects. Set if a valid pragma or aspect
--       Independent applies to the entity, or for a component if a valid
--       pragma or aspect Independent_Components applies to the enclosing
--       record type. Also set if a pragma Shared or pragma Atomic applies to
--       the entity, or if the declaration of the entity carries the Aliased
--       keyword. For Ada 2012, also applies to formal parameters. In the
--       case of private and incomplete types, this flag is set in both the
--       partial view and the full view.

--    Is_Initial_Condition_Procedure
--       Defined in functions and procedures. Set for a generated procedure
--       which verifies the assumption of pragma Initial_Condition at run time.

--    Is_Inlined
--       Defined in all entities. Set for functions and procedures which are
--       to be inlined. For subprograms created during expansion, this flag
--       may be set directly by the expander to request inlining. Also set
--       for packages that contain inlined subprograms, whose bodies must be
--       be compiled. Is_Inlined is also set on generic subprograms and is
--       inherited by their instances. It is also set on the body entities
--       of inlined subprograms. See also Has_Pragma_Inline.

--    Is_Inlined_Always
--       Defined in subprograms. Set for functions and procedures which are
--       always inlined in GNATprove mode. GNATprove uses this flag to know
--       when a body does not need to be analyzed. The value of this flag is
--       only meaningful if Body_To_Inline is not Empty for the subprogram.

--    Is_Instantiated
--       Defined in generic packages and generic subprograms. Set if the unit
--       is instantiated from somewhere in the extended main source unit. This
--       flag is used to control warnings about the unit being uninstantiated.
--       Also set in a package that is used as an actual for a generic package
--       formal in an instantiation. Also set on a parent instance, in the
--       instantiation of a child, which is implicitly declared in the parent.

--    Is_Integer_Type (synthesized)
--       Applies to all entities, true for integer types and subtypes

--    Is_Interface
--       Defined in record types and subtypes. Set to indicate that the current
--       entity corresponds to an abstract interface. Because abstract
--       interfaces are conceptually a special kind of abstract tagged type
--       we represent them by means of tagged record types and subtypes
--       marked with this attribute. This allows us to reuse most of the
--       compiler support for abstract tagged types to implement interfaces
--       (Ada 2005: AI-251).

--    Is_Internal
--       Defined in all entities. Set to indicate an entity created during
--       semantic processing (e.g. an implicit type, or a temporary). The
--       current uses of this flag are:
--
--         1) Internal entities (such as temporaries generated for the result
--         of an inlined function call or dummy variables generated for the
--         debugger). Set to indicate that they need not be initialized, even
--         when scalars are initialized or normalized.
--
--         2) Predefined primitives of tagged types. Set to mark that they
--         have specific properties: first they are primitives even if they
--         are not defined in the type scope (the freezing point is not
--         necessarily in the same scope), and second the predefined equality
--         can be overridden by a user-defined equality, no body will be
--         generated in this case.
--
--         3) Object declarations generated by the expander that are implicitly
--         imported or exported so that they can be marked in Sprint output.
--
--         4) Internal entities in the list of primitives of tagged types that
--         are used to handle secondary dispatch tables. These entities have
--         also the attribute Interface_Alias.

--    Is_Interrupt_Handler
--       Defined in procedures. Set if a pragma Interrupt_Handler applies
--       to the procedure. The procedure must be a parameterless protected
--       procedure.

--    Is_Intrinsic_Subprogram
--       Defined in functions and procedures. It is set if a valid pragma
--       Interface or Import is present for this subprogram specifying
--       convention Intrinsic. Valid means that the name and profile of the
--       subprogram match the requirements of one of the recognized intrinsic
--       subprograms (see package Sem_Intr for details). Note: the value of
--       Convention for such an entity will be set to Convention_Intrinsic,
--       but it is the setting of Is_Intrinsic_Subprogram, NOT simply having
--       convention set to intrinsic, which causes intrinsic code to be
--       generated.

--    Is_Invariant_Procedure
--       Defined in functions and procedures. Set for a generated invariant
--       procedure which verifies the invariants of both the partial and full
--       views of a private type or private extension as well as any inherited
--       class-wide invariants from parent types or interfaces.

--    Is_Itype
--       Defined in all entities. Set to indicate that a type is an Itype,
--       which means that the declaration for the type does not appear
--       explicitly in the tree. Instead the backend will elaborate the type
--       when it is first used. Has_Delayed_Freeze can be set for Itypes, and
--       the meaning is that the first use (the one which causes the type to be
--       defined) will be the freeze node. Note that an important restriction
--       on Itypes is that the first use of such a type (the one that causes it
--       to be defined) must be in the same scope as the type.

--    Is_Known_Non_Null
--       Defined in all entities. Relevant (and can be set) only for
--       objects of an access type. It is set if the object is currently
--       known to have a non-null value (meaning that no access checks
--       are needed). The indication can for example come from assignment
--       of an access parameter or an allocator whose value is known non-null.
--
--       Note: this flag is set according to the sequential flow of the
--       program, watching the current value of the variable. However, this
--       processing can miss cases of changing the value of an aliased or
--       constant object, so even if this flag is set, it should not be
--       believed if the variable is aliased or volatile. It would be a
--       little neater to avoid the flag being set in the first place in
--       such cases, but that's trickier, and there is only one place that
--       tests the value anyway.
--
--       The flag is dynamically set and reset as semantic analysis and
--       expansion proceeds. Its value is meaningless once the tree is
--       fully constructed, since it simply indicates the last state.
--       Thus this flag has no meaning to the backend.

--    Is_Known_Null
--       Defined in all entities. Relevant (and can be set ) only for
--       objects of an access type. It is set if the object is currently known
--       to have a null value (meaning that a dereference will surely raise
--       constraint error exception). The indication can come from an
--       assignment or object declaration.
--
--       The comments above about sequential flow and aliased and volatile for
--       the Is_Known_Non_Null flag apply equally to the Is_Known_Null flag.

--    Is_Known_Valid
--       Defined in all entities. Relevant for types (and subtype) and
--       for objects (and enumeration literals) of a discrete type.
--
--       The purpose of this flag is to implement the requirement stated
--       in (RM 13.9.1(9-11)) which require that the use of possibly invalid
--       values may not cause programs to become erroneous. See the function
--       Checks.Expr_Known_Valid for further details. Note that the setting
--       is conservative, in the sense that if the flag is set, it must be
--       right. If the flag is not set, nothing is known about the validity.
--
--       For enumeration literals, the flag is always set, since clearly
--       an enumeration literal represents a valid value. Range checks
--       where necessary will ensure that this valid value is appropriate.
--
--       For objects, the flag indicates the state of knowledge about the
--       current value of the object. This may be modified during expansion,
--       and thus the final value is not relevant to the backend.
--
--       For types and subtypes, the flag is set if all possible bit patterns
--       of length Object_Size (i.e. Esize of the type) represent valid values
--       of the type. In general for such types, all values are valid, the
--       only exception being the case where an object of the type has an
--       explicit size that is greater than Object_Size.
--
--       For non-discrete objects, the setting of the Is_Known_Valid flag is
--       not defined, and is not relevant, since the considerations of the
--       requirement in (RM 13.9.1(9-11)) do not apply.
--
--       The flag is dynamically set and reset as semantic analysis and
--       expansion proceeds. Its value is meaningless once the tree is
--       fully constructed, since it simply indicates the last state.
--       Thus this flag has no meaning to the backend.

--    Is_Limited_Composite
--       Defined in all entities. Set for composite types that have a limited
--       component. Used to enforce the rule that operations on the composite
--       type that depend on the full view of the component do not become
--       visible until the immediate scope of the composite type itself
--       (RM 7.3.1 (5)).

--    Is_Limited_Interface
--       Defined in record types and subtypes. True for interface types, if
--       interface is declared limited, task, protected, or synchronized, or
--       is derived from a limited interface.

--    Is_Limited_Record
--       Defined in all entities. Set to true for record (sub)types if the
--       record is declared to be limited. Note that this flag is not set
--       simply because some components of the record are limited.

--    Is_Local_Anonymous_Access
--       Defined in access types. Set for an anonymous access type to indicate
--       that the type is created for a record component with an access
--       definition, an array component, or (pre-Ada 2012) a standalone object.
--       Such anonymous types have an accessibility level equal to that of the
--       declaration in which they appear, unlike the anonymous access types
--       that are created for access parameters, access discriminants, and
--       (as of Ada 2012) stand-alone objects.

--    Is_Loop_Parameter
--       Applies to all entities. Certain loops, in particular "for ... of"
--       loops, get transformed so that the loop parameter is declared by a
--       variable declaration, so the entity is an E_Variable. This is True for
--       such E_Variables; False otherwise.

--    Is_Machine_Code_Subprogram
--       Defined in subprogram entities. Set to indicate that the subprogram
--       is a machine code subprogram (i.e. its body includes at least one
--       code statement). Also indicates that all necessary semantic checks
--       as required by RM 13.8(3) have been performed.

--    Is_Modular_Integer_Type (synthesized)
--       Applies to all entities. True if entity is a modular integer type

--    Is_Non_Static_Subtype
--       Defined in all type and subtype entities. It is set in some (but not
--       all) cases in which a subtype is known to be non-static. Before this
--       flag was added, the computation of whether a subtype was static was
--       entirely synthesized, by looking at the bounds, and the immediate
--       subtype parent. However, this method does not work for some Itypes
--       that have no parent set (and the only way to find the immediate
--       subtype parent is to go through the tree). For now, this flag is set
--       conservatively, i.e. if it is set then for sure the subtype is non-
--       static, but if it is not set, then the type may or may not be static.
--       Thus the test for a static subtype is that this flag is clear AND that
--       the bounds are static AND that the parent subtype (if available to be
--       tested) is static. Eventually we should make sure this flag is always
--       set right, at which point, these comments can be removed, and the
--       tests for static subtypes greatly simplified.

--    Is_Null_Init_Proc
--       Defined in procedure entities. Set for generated init proc procedures
--       (used to initialize composite types), if the code for the procedure
--       is null (i.e. is a return and nothing else). Such null initialization
--       procedures are generated in case some client is compiled using the
--       Initialize_Scalars pragma, generating a call to this null procedure,
--       but there is no need to call such procedures within a compilation
--       unit, and this flag is used to suppress such calls.

--    Is_Null_State (synthesized)
--       Applies to all entities, true for an abstract state declared with
--       keyword null.

--    Is_Numeric_Type (synthesized)
--       Applies to all entities, true for all numeric types and subtypes
--       (integer, fixed, float).

--    Is_Object (synthesized)
--       Applies to all entities, true for entities representing objects,
--       including generic formal parameters.

--    Is_Obsolescent
--       Defined in all entities. Set for any entity to which a valid pragma
--       or aspect Obsolescent applies.

--    Is_Only_Out_Parameter
--       Defined in formal parameter entities. Set if this parameter is the
--       only OUT parameter for this formal part. If there is more than one
--       out parameter, or if there is some other IN OUT parameter then this
--       flag is not set in any of them. Used in generation of warnings.

--    Is_Ordinary_Fixed_Point_Type (synthesized)
--       Applies to all entities, true for ordinary fixed point types and
--       subtypes.

--    Is_Package_Body_Entity
--       Defined in all entities. Set for entities defined at the top level
--       of a package body. Used to control externally generated names.

--    Is_Package_Or_Generic_Package (synthesized)
--       Applies to all entities. True for packages and generic packages.
--       False for all other entities.

--    Is_Packed [implementation base type only]
--       Defined in all type entities. This flag is set only for record and
--       array types which have a packed representation. There are four cases
--       which cause packing:
--
--         1. Explicit use of pragma Pack to pack a record.
--         2. Explicit use of pragma Pack to pack an array.
--         3. Setting Component_Size of an array to a packable value.
--         4. Indexing an array with a non-standard enumeration type.
--
--       For records, Is_Packed is always set if Has_Pragma_Pack is set, and
--       can also be set on its own in a derived type which inherited its
--       packed status.
--
--       For arrays, Is_Packed is set if either Has_Pragma_Pack is set and the
--       component size is either not known at compile time or known but not
--       8/16/32/64 bits, or a Component_Size clause exists and the specified
--       value is smaller than 64 bits but not 8/16/32, or if the array has one
--       or more index types that are enumeration types with a non-standard
--       representation (in GNAT, we store such arrays compactly, using the Pos
--       of the enumeration type value). As for the case of records, Is_Packed
--       can be set on its own for a derived type.

--       Before an array type is frozen, Is_Packed will always be set if
--       Has_Pragma_Pack is set. Before the freeze point, it is not possible
--       to know the component size, since the component type is not frozen
--       until the array type is frozen. Thus Is_Packed for an array type
--       before it is frozen means that packed is required. Then if it turns
--       out that the component size doesn't require packing, the Is_Packed
--       flag gets turned off.

--       In the bit-packed array case (i.e. the component size is known by the
--       front end and is in the range 1-63 but not a multiple of 8), then the
--       Is_Bit_Packed_Array flag will be set once the array type is frozen.
--
--    Is_Packed_Array (synth)
--       Applies to all entities, true if entity is for a packed array.

--    Is_Packed_Array_Impl_Type
--       Defined in all entities. This flag is set on the entity for the type
--       used to implement a packed array (either a modular type or a subtype
--       of Packed_Bytes{1,2,4} in the bit-packed array case, a regular array
--       in the non-standard enumeration index case). It is set if and only
--       if the type appears in the Packed_Array_Impl_Type field of some other
--       entity. It is used by the back end to activate the special processing
--       for such types (unchecked conversions that would not otherwise be
--       allowed are allowed for such types). If Is_Packed_Array_Impl_Type is
--       set in an entity, then the Original_Array_Type field of this entity
--       points to the array type for which this is the Packed_Array_Impl_Type.

--    Is_Param_Block_Component_Type [base type only]
--       Defined in access types. Set to indicate that a type is the type of a
--       component of the parameter block record type generated by the compiler
--       for an entry or a select statement. Read by CodePeer.

--    Is_Partial_Invariant_Procedure
--       Defined in functions and procedures. Set for a generated invariant
--       procedure which verifies the invariants of the partial view of a
--       private type or private extension.

--    Is_Potentially_Use_Visible
--       Defined in all entities. Set if entity is potentially use visible,
--       i.e. it is defined in a package that appears in a currently active
--       use clause (RM 8.4(8)). Note that potentially use visible entities
--       are not necessarily use visible (RM 8.4(9-11)).

--    Is_Predicate_Function
--       Present in functions and procedures. Set for generated predicate
--       functions.

--    Is_Preelaborated
--       Defined in all entities, set in E_Package and E_Generic_Package
--       entities to which a pragma Preelaborate is applied, and also in
--       all entities within such packages. Note that the fact that this
--       flag is set does not necessarily mean that no elaboration code is
--       generated for the package.

--    Is_Primitive
--       Defined in overloadable entities and in generic subprograms. Set to
--       indicate that this is a primitive operation of some type, which may
--       be a tagged type or an untagged type. Used to verify overriding
--       indicators in bodies.

--    Is_Primitive_Wrapper
--       Defined in functions and procedures created by the expander to serve
--       as an indirection mechanism to overriding primitives of concurrent
--       types, entries and protected procedures.

--    Is_Prival (synthesized)
--       Applies to all entities, true for renamings of private protected
--       components. Such entities appear as constants or variables.

--    Is_Private_Composite
--       Defined in composite types that have a private component. Used to
--       enforce the rule that operations on the composite type that depend
--       on the full view of the component, do not become visible until the
--       immediate scope of the composite type itself (7.3.1 (5)). Both this
--       flag and Is_Limited_Composite are needed.

--    Is_Private_Descendant
--       Defined in entities that can represent library units (packages,
--       functions, procedures). Set if the library unit is itself a private
--       child unit, or if it is the descendant of a private child unit.

--    Is_Private_Primitive
--       Defined in subprograms. Set if the operation is a primitive of a
--       tagged type (procedure or function dispatching on result) whose
--       full view has not been seen. Used in particular for primitive
--       subprograms of a synchronized type declared between the two views
--       of the type, so that the wrapper built for such a subprogram can
--       be given the proper signature.

--    Is_Private_Type (synthesized)
--       Applies to all entities, true for private types and subtypes,
--       as well as for record with private types as subtypes.

--    Is_Protected_Component (synthesized)
--       Applicable to all entities, true if the entity denotes a private
--       component of a protected type.

--    Is_Protected_Interface (synthesized)
--       Defined in types that are interfaces. True if interface is declared
--       protected, or is derived from protected interfaces.

--    Is_Protected_Record_Type (synthesized)
--       Applies to all entities, true if Is_Concurrent_Record_Type is true and
--       Corresponding_Concurrent_Type is a protected type.

--    Is_Protected_Type (synthesized)
--       Applies to all entities, true for protected types and subtypes

--    Is_Public
--       Defined in all entities. Set to indicate that an entity defined in
--       one compilation unit can be referenced from other compilation units.
--       If this reference causes a reference in the generated code, for
--       example in the case of a variable name, then the backend will generate
--       an appropriate external name for use by the linker.

--    Is_Pure
--       Defined in all entities. Set in all entities of a unit to which a
--       pragma Pure is applied except for non-intrinsic imported subprograms,
--       and also set for the entity of the unit itself. In addition, this
--       flag may be set for any other functions or procedures that are known
--       to be side effect free, so in the case of subprograms, the Is_Pure
--       flag may be used by the optimizer to imply that it can assume freedom
--       from side effects (other than those resulting from assignment to Out
--       or In Out parameters, or to objects designated by access parameters).

--    Is_Pure_Unit_Access_Type
--       Defined in access type and subtype entities. Set if the type or
--       subtype appears in a pure unit. Used to give an error message at
--       freeze time if the access type has a storage pool.

--    Is_RACW_Stub_Type
--       Defined in all types, true for the stub types generated for remote
--       access-to-class-wide types.

--    Is_Raised
--       Defined in exception entities. Set if the entity is referenced by a
--       a raise statement.

--    Is_Real_Type (synthesized)
--       Applies to all entities, true for real types and subtypes

--    Is_Record_Type (synthesized)
--       Applies to all entities, true for record types and subtypes,
--       includes class-wide types and subtypes (which are also records).

--    Is_Relaxed_Initialization_State (synthesized)
--       Applies to all entities, true for abstract states that are subject to
--       option Relaxed_Initialization.

--    Is_Remote_Call_Interface
--       Defined in all entities. Set in E_Package and E_Generic_Package
--       entities to which a pragma Remote_Call_Interface is applied, and
--       also on entities declared in the visible part of such a package.

--    Is_Remote_Types
--       Defined in all entities. Set in E_Package and E_Generic_Package
--       entities to which a pragma Remote_Types is applied, and also on
--       entities declared in the visible part of the spec of such a package.
--       Also set for types which are generic formal types to which the
--       pragma Remote_Access_Type applies.

--    Is_Renaming_Of_Object
--       Defined in all entities, set only for a variable or constant for
--       which the Renamed_Object field is non-empty and for which the
--       renaming is handled by the front end, by macro substitution of
--       a copy of the (evaluated) name tree wherever the variable is used.

--    Is_Return_Object
--       Defined in all object entities. True if the object is the return
--       object of an extended_return_statement; False otherwise.

--    Is_Safe_To_Reevaluate
--       Defined in all entities. Set in variables that are initialized by
--       means of an assignment statement. When initialized their contents
--       never change and hence they can be seen by the backend as constants.
--       See also Is_True_Constant.

--    Is_Scalar_Type (synthesized)
--       Applies to all entities, true for scalar types and subtypes

--    Is_Shared_Passive
--       Defined in all entities. Set in E_Package and E_Generic_Package
--       entities to which a pragma Shared_Passive is applied, and also in
--       all entities within such packages.

--    Is_Standard_Character_Type (synthesized)
--       Applies to all entities, true for types and subtypes whose root type
--       is one of the standard character types (Character, Wide_Character, or
--       Wide_Wide_Character).

--    Is_Standard_String_Type (synthesized)
--       Applies to all entities, true for types and subtypes whose root
--       type is one of the standard string types (String, Wide_String, or
--       Wide_Wide_String).

--    Is_Static_Type
--       Defined in entities. Only set for (sub)types. If set, indicates that
--       the type is known to be a static type (defined as a discrete type with
--       static bounds, a record all of whose component types are static types,
--       or an array, all of whose bounds are of a static type, and also have
--       a component type that is a static type). See Set_Uplevel_Type for more
--       information on how this flag is used.

--    Is_Statically_Allocated
--       Defined in all entities. This can only be set for exception,
--       variable, constant, and type/subtype entities. If the flag is set,
--       then the variable or constant must be allocated statically rather
--       than on the local stack frame. For exceptions, the meaning is that
--       the exception data should be allocated statically (and indeed this
--       flag is always set for exceptions, since exceptions do not have
--       local scope). For a type, the meaning is that the type must be
--       elaborated at the global level rather than locally. No type marked
--       with this flag may depend on a local variable, or on any other type
--       which does not also have this flag set to True. For a variable or
--       or constant, if the flag is set, then the type of the object must
--       either be declared at the library level, or it must also have the
--       flag set (since to allocate the object statically, its type must
--       also be elaborated globally).

--    Is_String_Type (synthesized)
--       Applies to all type entities. Determines if the given type is a
--       string type, i.e. it is directly a string type or string subtype,
--       or a string slice type, or an array type with one dimension and a
--       component type that is a character type.

--    Is_Subprogram (synthesized)
--       Applies to all entities, true for function, procedure and operator
--       entities.

--    Is_Subprogram_Or_Generic_Subprogram
--       Applies to all entities, true for function procedure and operator
--       entities, and also for the corresponding generic entities.

--    Is_Synchronized_Interface (synthesized)
--       Defined in types that are interfaces. True if interface is declared
--       synchronized, task, or protected, or is derived from a synchronized
--       interface.

--    Is_Synchronized_State (synthesized)
--       Applies to all entities, true for abstract states that are subject to
--       option Synchronous.

--    Is_Tag
--       Defined in E_Component and E_Constant entities. For regular tagged
--       type this flag is set on the tag component (whose name is Name_uTag).
--       For CPP_Class tagged types, this flag marks the pointer to the main
--       vtable (i.e. the one to be extended by derivation).

--    Is_Tagged_Type
--       Defined in all entities, set for an entity that is a tagged type

--    Is_Task_Interface (synthesized)
--       Defined in types that are interfaces. True if interface is declared as
--       a task interface, or if it is derived from task interfaces.

--    Is_Task_Record_Type (synthesized)
--       Applies to all entities, true if Is_Concurrent_Record_Type is true and
--       Corresponding_Concurrent_Type is a task type.

--    Is_Task_Type (synthesized)
--       Applies to all entities. True for task types and subtypes

--    Is_Thunk
--       Defined in all entities. True for subprograms that are thunks, that is
--       small subprograms built by the expander for particular tagged types.
--       There are two different kinds of thunk: interface thunk and secondary
--       stack thunk. Interface thunks are built for tagged types that cover
--       interface types. As part of the runtime call to an interface, they
--       displace the pointer to the object (pointer named "this" in the C++
--       terminology) from a secondary dispatch table to the primary dispatch
--       table associated with a given tagged type; if the thunk is a function
--       that returns an object which covers an interface type then the thunk
--       displaces the pointer to the object from the primary dispatch table to
--       the secondary dispatch table associated with the interface type.

--       Secondary stack thunks are built for tagged types that do not need to
--       be returned on the secondary stack but have primitive functions which
--       can dispatch on result. In this case, dispatching calls made to these
--       primitive functions nevertheless need to return on the secondary stack
--       and a thunk is built to move the result from the primary stack onto
--       the secondary stack on return from the primitive function. The flag
--       Has_Controlling_Result is set on secondary stack thunks but not on
--       interface thunks.

--       Thunks may be chained in a single way: an interface thunk may point to
--       a secondary stack thunk, which points to the final thunk target.

--    Is_Trivial_Subprogram
--       Defined in all entities. Set in subprograms where either the body
--       consists of a single null statement, or the first or only statement
--       of the body raises an exception. This is used for suppressing certain
--       warnings, see Sem_Ch6.Analyze_Subprogram_Body discussion for details.

--    Is_True_Constant
--       Defined in all entities for constants and variables. Set in constants
--       and variables which have an initial value specified but which are
--       never assigned, partially or in the whole. For variables, it means
--       that the variable was initialized but never modified, and hence can be
--       treated as a constant by the code generator. For a constant, it means
--       that the constant was not modified by generated code (e.g. to set a
--       discriminant in an init proc). Assignments by user or generated code
--       will reset this flag. See also Is_Safe_To_Reevaluate.

--    Is_Type (synthesized)
--       Applies to all entities, true for a type entity

--    Is_Unchecked_Union [implementation base type only]
--       Defined in all entities. Set only in record types to which the
--       pragma Unchecked_Union has been validly applied.

--    Is_Underlying_Full_View
--       Defined in all entities. Set for types which represent the true full
--       view of a private type completed by another private type. For further
--       details, see attribute Underlying_Full_View.

--    Is_Underlying_Record_View [base type only]
--       Defined in all entities. Set only in record types that represent the
--       underlying record view. This view is built for derivations of types
--       with unknown discriminants; it is a record with the same structure
--       as its corresponding record type, but whose parent is the full view
--       of the parent in the original type extension.

--    Is_Unimplemented
--       Defined in all entities. Set for any entity to which a valid pragma
--       or aspect Unimplemented applies.

--    Is_Unsigned_Type
--       Defined in all types, but can be set only for discrete and fixed-point
--       type and subtype entities. This flag is only valid if the entity is
--       frozen. If set it indicates that the representation is known to be
--       unsigned (i.e. that no negative values appear in the range). This is
--       normally just a reflection of the lower bound of the subtype or base
--       type, but there is one case in which the setting is not obvious,
--       namely the case of an unsigned subtype of a signed type from which
--       a further subtype is obtained using variable bounds. This further
--       subtype is still unsigned, but this cannot be determined by looking
--       at its bounds or the bounds of the corresponding base type.
--       For a subtype indication whose range is statically a null range,
--       the flag is set if the lower bound is non-negative, but the flag
--       cannot be used to determine the comparison operator to emit in the
--       generated code: use the base type.

--    Is_Uplevel_Referenced_Entity
--       Defined in all entities. Used when unnesting subprograms to indicate
--       that an entity is locally defined within a subprogram P, and there is
--       a reference to the entity within a subprogram nested within P (at any
--       depth). Set for uplevel referenced objects (variables, constants,
--       discriminants and loop parameters), and also for upreferenced dynamic
--       types, including the cases where the reference is implicit (e.g. the
--       type of an array used for computing the location of an element in an
--       array. This is used internally in Exp_Unst, see this package for
--       further details.

--    Is_Valued_Procedure
--       Defined in procedure entities. Set if an Import_Valued_Procedure
--       or Export_Valued_Procedure pragma applies to the procedure entity.

--    Is_Visible_Formal
--       Defined in all entities. Set for instances of the formals of a
--       formal package. Indicates that the entity must be made visible in the
--       body of the instance, to reproduce the visibility of the generic.
--       This simplifies visibility settings in instance bodies.

--    Is_Visible_Lib_Unit
--       Defined in all (root or child) library unit entities. Once compiled,
--       library units remain chained to the entities in the parent scope, and
--       a separate flag must be used to indicate whether the names are visible
--       by selected notation, or not.

--    Is_Volatile
--       Defined in all type entities, and also in constants, components and
--       variables. Set if a pragma Volatile applies to the entity. Also set
--       if pragma Shared or pragma Atomic applies to entity. In the case of
--       private or incomplete types, this flag is set in both the private
--       and full view. The flag is not set reliably on private subtypes,
--       and is always retrieved from the base type (but this is not a base-
--       type-only attribute because it applies to other entities). Note that
--       the backend should use Treat_As_Volatile, rather than Is_Volatile
--       to indicate code generation requirements for volatile variables.
--       Similarly, any front end test which is concerned with suppressing
--       optimizations on volatile objects should test Treat_As_Volatile
--       rather than testing this flag.
--       This is a synthesized attribute in Einfo.Utils, based on
--       Is_Volatile_Type and Is_Volatile_Object. The latter two should be
--       used in preference to Is_Volatile when we know that we have a type
--       or an object.

--    Is_Volatile_Full_Access
--       Defined in all type entities, and also in constants, components, and
--       variables. Set if an aspect/pragma Volatile_Full_Access or an Ada 2022
--       aspect Full_Access_Only applies to the entity. In the case of private
--       and incomplete types, this flag is set in both the partial view and
--       the full view.

--    Is_Wrapper_Package (synthesized)
--       Defined in package entities. Indicates that the package has been
--       created as a wrapper for a subprogram instantiation.

--    Is_Wrapper
--       Defined in subprogram entities. Indicates that it has been created as
--       a wrapper to handle inherited class-wide pre/post conditions that call
--       overridden primitives or as a wrapper of a controlling function.

--    Itype_Printed
--       Defined in all type and subtype entities. Set in Itypes if the Itype
--       has been printed by Sprint. This is used to avoid printing an Itype
--       more than once.

--    Kill_Elaboration_Checks
--       Defined in all entities. Set by the expander to kill elaboration
--       checks which are known not to be needed. Equivalent in effect to
--       the use of pragma Suppress (Elaboration_Checks) for that entity
--       except that the effect is permanent and cannot be undone by a
--       subsequent pragma Unsuppress.

--    Kill_Range_Checks
--       Defined in all entities. Equivalent in effect to the use of pragma
--       Suppress (Range_Checks) for that entity except that the result is
--       permanent and cannot be undone by a subsequent pragma Unsuppress.
--       This is currently only used in one odd situation in Sem_Ch3 for
--       record types, and it would be good to get rid of it???

--    Known_To_Have_Preelab_Init
--       Defined in all type and subtype entities. If set, then the type is
--       known to have preelaborable initialization. In the case of a partial
--       view of a private type, it is only possible for this to be set if a
--       pragma Preelaborable_Initialization is given for the type. For other
--       types, it is never set if the type does not have preelaborable
--       initialization, it may or may not be set if the type does have
--       preelaborable initialization.

--    Last_Aggregate_Assignment
--       Applies to controlled constants and variables initialized by an
--       aggregate. Points to the last statement associated with the expansion
--       of the aggregate. The attribute is used by the finalization machinery
--       when marking an object as successfully initialized.

--    Last_Assignment
--       Defined in entities for variables, and OUT or IN OUT formals. Set for
--       a local variable or formal to point to the left side of an assignment
--       statement assigning a value to the variable. Cleared if the value of
--       the entity is referenced. Used to warn about dubious assignment
--       statements whose value is not used.

--    Last_Entity
--       Defined in all entities which act as scopes to which a list of
--       associated entities is attached (blocks, class subtypes and types,
--       entries, functions, loops, packages, procedures, protected objects,
--       record types and subtypes, private types, task types and subtypes).
--       Points to the last entry in the list of associated entities chained
--       through the Next_Entity field. Empty if no entities are chained.

--    Last_Formal (synthesized)
--       Applies to subprograms and subprogram types, and also in entries
--       and entry families. Returns last formal of the subprogram or entry.
--       The formals are the first entities declared in a subprogram or in
--       a subprogram type (the designated type of an Access_To_Subprogram
--       definition) or in an entry.

--    Limited_View
--       Defined in non-generic package entities that are not instances. Bona
--       fide package with the limited-view list through the first_entity and
--       first_private attributes. The elements of this list are the shadow
--       entities created for the types and local packages that are declared
--       in a package appearing in a limited_with clause (Ada 2005: AI-50217).

--    Linker_Section_Pragma
--       Present in constant, variable, type and subprogram entities. Points
--       to a linker section pragma that applies to the entity, or is Empty if
--       no such pragma applies. Note that for constants and variables, this
--       field may be set as a result of a linker section pragma applied to the
--       type of the object.

--    Lit_Hash
--       Defined in enumeration types and subtypes. Non-empty only for the
--       case of an enumeration root type, where it contains the entity for
--       the generated hash function. See unit Exp_Imgv for full details of
--       the nature and use of this entity for implementing the Value
--       attribute for the enumeration type in question.

--    Lit_Indexes
--       Defined in enumeration types and subtypes. Non-empty only for the
--       case of an enumeration root type, where it contains the entity for
--       the generated indexes entity. See unit Exp_Imgv for full details of
--       the nature and use of this entity for implementing the Image and
--       Value attributes for the enumeration type in question.

--    Lit_Strings
--       Defined in enumeration types and subtypes. Non-empty only for the
--       case of an enumeration root type, where it contains the entity for
--       the literals string entity. See unit Exp_Imgv for full details of
--       the nature and use of this entity for implementing the Image and
--       Value attributes for the enumeration type in question.

--    Low_Bound_Tested
--       Defined in all entities. Currently this can only be set for formal
--       parameter entries of a standard unconstrained one-dimensional array
--       or string type. Indicates that an explicit test of the low bound of
--       the formal appeared in the code, e.g. in a pragma Assert. If this
--       flag is set, warnings about assuming the index low bound to be one
--       are suppressed.

--    Machine_Radix_10
--       Defined in decimal types and subtypes, set if the Machine_Radix is 10,
--       as the result of the specification of a machine radix representation
--       clause. Note that it is possible for this flag to be set without
--       having Has_Machine_Radix_Clause True. This happens when a type is
--       derived from a type with a clause present.

--    Master_Id
--       Defined in access types and subtypes. Empty unless Has_Task is set for
--       the designated type, in which case it points to the entity for the
--       Master_Id for the access type master. Also set for access-to-limited-
--       class-wide types whose root may be extended with task components, and
--       for access-to-limited-interfaces because they can be used to reference
--       tasks implementing such interface.

--    Materialize_Entity
--       Defined in all entities. Set only for renamed obects which should be
--       materialized for debugging purposes. This means that a memory location
--       containing the renamed address should be allocated. This is needed so
--       that the debugger can find the entity.

--    May_Inherit_Delayed_Rep_Aspects
--       Defined in all entities for types and subtypes. Set if the type is
--       derived from a type which has delayed rep aspects (marked by the flag
--       Has_Delayed_Rep_Aspects being set). In this case, at the freeze point
--       for the derived type we know that the parent type is frozen, and if
--       a given attribute has not been set for the derived type, we copy the
--       value from the parent type. See Freeze.Inherit_Delayed_Rep_Aspects.

--    Mechanism (returned as Mechanism_Type)
--       Defined in functions and non-generic formal parameters. Indicates
--       the mechanism to be used for the function return or for the formal
--       parameter. See full description in the spec of Sem_Mech. This field
--       is also set (to the default value of zero = Default_Mechanism) in a
--       subprogram body entity but not used in this context.

--    Minimum_Accessibility
--       Defined in formal parameters in the non-generic case. Normally Empty,
--       but if expansion is active, and a parameter exists for which a
--       dynamic accessibility check is required, then an object is generated
--       within such a subprogram representing the accessibility level of the
--       subprogram or the formal's Extra_Accessibility - whichever one is
--       lesser. The Minimum_Accessibility field then points to this object.

--    Modulus [base type only]
--       Defined in modular types. Contains the modulus. For the binary case,
--       this will be a power of 2, but if Non_Binary_Modulus is set, then it
--       will not be a power of 2.

--    Must_Be_On_Byte_Boundary
--       Defined in entities for types and subtypes. Set if objects of the type
--       must always be allocated on a byte boundary (more accurately a storage
--       unit boundary). The front end checks that component clauses respect
--       this rule, and the backend ensures that record packing does not
--       violate this rule. Currently the flag is set only for packed arrays
--       longer than 64 bits where the component size is not a power of 2.

--    Must_Have_Preelab_Init
--       Defined in entities for types and subtypes. Set in the full type of a
--       private type or subtype if a pragma Has_Preelaborable_Initialization
--       is present for the private type. Used to check that the full type has
--       preelaborable initialization at freeze time (this has to be deferred
--       to the freeze point because of the rule about overriding Initialize).

--    Needs_Activation_Record
--       Defined on generated subprogram types. Indicates that a call through
--       a named or anonymous access to subprogram requires an activation
--       record when compiling with unnesting for C or LLVM.

--    Needs_Debug_Info
--       Defined in all entities. Set if the entity requires normal debugging
--       information to be generated. This is true of all entities that have
--       Comes_From_Source set, and also transitively for entities associated
--       with such components (e.g. their types). It is true for all entities
--       in Debug_Generated_Code mode (-gnatD switch). This is the flag that
--       the backend should check to determine whether or not to generate
--       debugging information for an entity. Note that callers should always
--       use Sem_Util.Set_Debug_Info_Needed, rather than Set_Needs_Debug_Info,
--       so that the flag is set properly on subsidiary entities.

--    Needs_No_Actuals
--       Defined in callable entities (subprograms, entries, access to
--       subprograms) which can be called without actuals because all of
--       their formals (if any) have default values. This flag simplifies the
--       resolution of the syntactic ambiguity involving a call to these
--       entities when the return type is an array type, and a call can be
--       interpreted as an indexing of the result of the call. It is also
--       used to resolve various cases of entry calls.

--    Never_Set_In_Source
--       Defined in all entities, but can be set only for variables and
--       parameters. This flag is set if the object is never assigned a value
--       in user source code, either by assignment or by being used as an out
--       or in out parameter. Note that this flag is not reset from using an
--       initial value, so if you want to test for this case as well, test the
--       Has_Initial_Value flag also.
--
--       This flag is only for the purposes of issuing warnings, it must not
--       be used by the code generator to indicate that the variable is in
--       fact a constant, since some assignments in generated code do not
--       count (for example, the call to an init proc to assign some but
--       not all of the fields in a partially initialized record). The code
--       generator should instead use the flag Is_True_Constant.
--
--       For the purposes of this warning, the default assignment of access
--       variables to null is not considered the assignment of a value (so
--       the warning can be given for code that relies on this initial null
--       value when no other value is ever set).
--
--       In variables and out parameters, if this flag is set after full
--       processing of the corresponding declarative unit, it indicates that
--       the variable or parameter was never set, and a warning message can
--       be issued.
--
--       Note: this flag is initially set, and then cleared on encountering
--       any construct that might conceivably legitimately set the value.
--       Thus during the analysis of a declarative region and its associated
--       statement sequence, the meaning of the flag is "not set yet", and
--       once this analysis is complete the flag means "never assigned".

--       Note: for variables appearing in package declarations, this flag is
--       never set. That is because there is no way to tell if some client
--       modifies the variable (or, in the case of variables in the private
--       part, if some child unit modifies the variables).

--       Note: in the case of renamed objects, the flag must be set in the
--       ultimate renamed object. Clients noting a possible modification
--       should use the Note_Possible_Modification procedure in Sem_Util
--       rather than Set_Never_Set_In_Source precisely to deal properly with
--       the renaming possibility.

--    Next_Component (synthesized)
--       Applies to record components. Returns the next component by following
--       the chain of declared entities until one is found which corresponds to
--       a component (Ekind is E_Component). Any internal types generated from
--       the subtype indications of the record components are skipped. Returns
--       Empty if no more components.

--    Next_Component_Or_Discriminant (synthesized)
--      Similar to Next_Component, but includes components and discriminants
--      so the input can have either E_Component or E_Discriminant, and the
--      same is true for the result. Returns Empty if no more components or
--      discriminants in the record.

--    Next_Discriminant (synthesized)
--       Applies to discriminants returned by First/Next_Discriminant. Returns
--       the next language-defined (i.e. perhaps non-stored) discriminant by
--       following the chain of declared entities as long as the kind of the
--       entity corresponds to a discriminant. Note that the discriminants
--       might be the only components of the record. Returns Empty if there
--       are no more discriminants.

--    Next_Entity
--       Defined in all entities. The entities of a scope are chained, with
--       the head of the list being in the First_Entity field of the scope
--       entity. All entities use the Next_Entity field as a forward pointer
--       for this list, with Empty indicating the end of the list. Since this
--       field is in the base part of the entity, the access routines for this
--       field are in Sinfo.

--    Next_Formal (synthesized)
--       Applies to the entity for a formal parameter. Returns the next formal
--       parameter of the subprogram or subprogram type. Returns Empty if there
--       are no more formals.

--    Next_Formal_With_Extras (synthesized)
--       Applies to the entity for a formal parameter. Returns the next
--       formal parameter of the subprogram or subprogram type. Returns
--       Empty if there are no more formals. The list returned includes
--       all the extra formals (see description of Extra_Formal field)

--    Next_Index (synthesized)
--       Applies to array types and subtypes and to string types and
--       subtypes. Yields the next index. The first index is obtained by
--       using the First_Index attribute, and then subsequent indexes are
--       obtained by applying Next_Index to the previous index. Empty is
--       returned to indicate that there are no more indexes. Note that
--       unlike most attributes in this package, Next_Index applies to
--       nodes for the indexes, not to entities.

--    Next_Inlined_Subprogram
--       Defined in subprograms. Used to chain inlined subprograms used in
--       the current compilation, in the order in which they must be compiled
--       by the backend to ensure that all inlinings are performed.

--    Next_Literal (synthesized)
--       Applies to enumeration literals, returns the next literal, or
--       Empty if applied to the last literal. This is actually a synonym
--       for Next, but its use is preferred in this context.

--    No_Dynamic_Predicate_On_Actual
--       Defined in discrete types. Set for generic formal types that are used
--       in loops and quantified expressions. The corresponding actual cannot
--       have dynamic predicates.

--    No_Pool_Assigned [root type only]
--       Defined in access types. Set if a storage size clause applies to the
--       variable with a static expression value of zero. This flag is used to
--       generate errors if any attempt is made to allocate or free an instance
--       of such an access type. This is set only in the root type, since
--       derived types must have the same pool.

--    No_Predicate_On_Actual
--       Defined in discrete types. Set for generic formal types that are used
--       in the spec of a generic package, in constructs that forbid discrete
--       types with predicates.

--    No_Reordering [implementation base type only]
--       Defined in record types. Set only for a base type to which a valid
--       pragma No_Component_Reordering applies.

--    No_Return
--       Defined in all entities. Set for subprograms and generic subprograms
--       to which a valid aspect or pragma No_Return applies.

--    No_Strict_Aliasing [base type only]
--       Defined in access types. Set to direct the backend to avoid any
--       optimizations based on an assumption about the aliasing status of
--       objects designated by the access type. For the case of the gcc
--       backend, the effect is as though all references to objects of
--       the type were compiled with -fno-strict-aliasing. This flag is
--       set if an unchecked conversion with the access type as a target
--       type occurs in the same source unit as the declaration of the
--       access type, or if an explicit pragma No_Strict_Aliasing applies.

--    No_Tagged_Streams_Pragma
--       Present in all subtype and type entities. Set for tagged types and
--       subtypes (i.e. entities with Is_Tagged_Type set True) if a valid
--       pragma/aspect applies to the type.

--    Non_Binary_Modulus [base type only]
--       Defined in all subtype and type entities. Set for modular integer
--       types if the modulus value is other than a power of 2.

--    Non_Limited_View
--       Defined in abstract states and incomplete types that act as shadow
--       entities created when analysing a limited with clause (Ada 2005:
--       AI-50217). Points to the defining entity of the original declaration.

--    Nonzero_Is_True [base type only]
--       Defined in enumeration types. Set if any non-zero value is to be
--       interpreted as true. Currently this is set for derived Boolean
--       types which have a convention of C, C++ or Fortran.

--    Normalized_First_Bit
--       Defined in components and discriminants. Indicates the normalized
--       value of First_Bit for the component, i.e. the offset within the
--       lowest addressed storage unit containing part or all of the field.
--       Set to No_Uint if no first bit position is assigned yet.

--    Normalized_Position
--       Defined in components and discriminants. Indicates the normalized
--       value of Position for the component, i.e. the offset in storage
--       units from the start of the record to the lowest addressed storage
--       unit containing part or all of the field.

--    Number_Dimensions (synthesized)
--       Applies to array types and subtypes. Returns the number of dimensions
--       of the array type or subtype as a value of type Pos.

--    Number_Entries (synthesized)
--       Applies to concurrent types. Returns the number of entries that are
--       declared within the task or protected definition for the type.

--    Number_Formals (synthesized)
--       Applies to subprograms and subprogram types. Yields the number of
--       formals as a value of type Pos.

--    Object_Size_Clause (synthesized)
--       Applies to entities for types and subtypes. If an object size clause
--       is present in the rep item chain for an entity then the attribute
--       definition clause node is returned. Otherwise Object_Size_Clause
--       returns Empty if no item is present. Usually this is only meaningful
--       if the flag Has_Object_Size_Clause is set. This is because when the
--       representation item chain is copied for a derived type, it can inherit
--       an object size clause that is not applicable to the entity.

--    OK_To_Rename
--       Defined only in entities for variables. If this flag is set, it
--       means that if the entity is used as the initial value of an object
--       declaration, the object declaration can be safely converted into a
--       renaming to avoid an extra copy. This is set for variables which are
--       generated by the expander to hold the result of evaluating some
--       expression. Most notably, the local variables used to store the result
--       of concatenations are so marked (see Exp_Ch4.Expand_Concatenate). It
--       is only worth setting this flag for composites, since for primitive
--       types, it is cheaper to do the copy.

--    Optimize_Alignment_Space
--       Defined in type, subtype, variable, and constant entities. This
--       flag records that the type or object is to be laid out in a manner
--       consistent with Optimize_Alignment (Space) mode. The compiler and
--       binder ensure a consistent view of any given type or object. If pragma
--       Optimize_Alignment (Off) mode applies to the type/object, then neither
--       of the flags Optimize_Alignment_Space/Optimize_Alignment_Time is set.

--    Optimize_Alignment_Time
--       Defined in type, subtype, variable, and constant entities. This
--       flag records that the type or object is to be laid out in a manner
--       consistent with Optimize_Alignment (Time) mode. The compiler and
--       binder ensure a consistent view of any given type or object. If pragma
--       Optimize_Alignment (Off) mode applies to the type/object, then neither
--       of the flags Optimize_Alignment_Space/Optimize_Alignment_Time is set.

--    Original_Access_Type
--       Defined in E_Access_Subprogram_Type entities. Set only if the access
--       type was generated by the expander as part of processing an access-
--       to-protected-subprogram type. Points to the access-to-protected-
--       subprogram type.

--    Original_Array_Type
--       Defined in modular types and array types and subtypes. Set only if
--       the Is_Packed_Array_Impl_Type flag is set, indicating that the type
--       is the implementation type for a packed array, and in this case it
--       points to the original array type for which this is the packed
--       array implementation type.

--    Original_Protected_Subprogram
--       Defined in functions and procedures. Set only on internally built
--       dispatching subprograms of protected types to reference their original
--       non-dispatching protected subprogram since their names differ.

--    Original_Record_Component
--       Defined in components, including discriminants. The usage depends
--       on whether the record is a base type and whether it is tagged.
--
--       In base tagged types:
--         When the component is inherited in a record extension, it points
--         to the original component (the entity of the ancestor component
--         which is not itself inherited) otherwise it points to itself. The
--         backend uses this attribute to implement the automatic dereference
--         in the extension and to apply the transformation:
--
--            Rec_Ext.Comp -> Rec_Ext.Parent. ... .Parent.Comp
--
--       In base untagged types:
--         Always points to itself except for non-stored discriminants, where
--         it points to the stored discriminant it renames.
--
--       In subtypes (tagged and untagged):
--         Points to the component in the base type.

--    Overlays_Constant
--       Defined in all entities. Set only for E_Constant or E_Variable for
--       which there is an address clause that causes the entity to overlay
--       a constant object.

--    Overridden_Operation
--       Defined in subprograms. For overriding operations, points to the
--       user-defined parent subprogram that is being overridden. Note: this
--       attribute uses the same field as Static_Initialization. The latter
--       is only defined for internal initialization procedures, for which
--       Overridden_Operation is irrelevant. Thus this attribute must not be
--       set for init_procs.

--    Package_Instantiation
--       Defined in packages and generic packages. When defined, this field
--       references an N_Generic_Instantiation node associated with an
--       instantiated package. In the case where the referenced node has
--       been rewritten to an N_Package_Specification, the instantiation
--       node is available from the Original_Node field of the package spec
--       node. This is currently not guaranteed to be set in all cases, but
--       when set, the field is used in Get_Unit_Instantiation_Node as
--       one of the means of obtaining the instantiation node. Eventually
--       it should be set in all cases, including package entities associated
--       with formal packages. ???

--    Packed_Array_Impl_Type
--       Defined in array types and subtypes, except for the string literal
--       subtype case, if the corresponding type is packed and implemented
--       specially (either bit-packed or packed to eliminate holes in the
--       non-contiguous enumeration index types). References the type used to
--       represent the packed array, which is either a modular type for short
--       static arrays or an array of System.Unsigned in the bit-packed case,
--       or a regular array in the non-standard enumeration index case. Note
--       that in some situations (internal types and references to fields of
--       variant records), it is not always possible to construct this type in
--       advance of its use. If this field is empty, then the necessary type
--       is declared on the fly for each reference to the array.

--    Parameter_Mode (synthesized)
--       Applies to formal parameter entities. This is a synonym for Ekind,
--       used when obtaining the formal kind of a formal parameter (the result
--       is one of E_[In/Out/In_Out]_Parameter).

--    Parent_Subtype [base type only]
--       Defined in E_Record_Type. Set only for derived tagged types, in which
--       case it points to the subtype of the parent type. This is the type
--       that is used as the Etype of the _parent field.

--    Part_Of_Constituents
--       Present in abstract state and variable entities. Contains all
--       constituents that are subject to indicator Part_Of (both aspect and
--       option variants).

--    Part_Of_References
--       Present in variable entities. Contains all references to the variable
--       when it is subject to pragma Part_Of. If the variable is a constituent
--       of a single protected/task type, the references are examined as they
--       must appear only within the type definition and the corresponding
--       body.

--    Partial_DIC_Procedure (synthesized)
--       Defined in type entities. Set for a private type and its full view
--       when the type is subject to pragma Default_Initial_Condition (DIC), or
--       when the type inherits a DIC pragma from a parent type. Points to the
--       entity of a procedure that takes a single argument of the given type
--       and verifies the assertion expression of the DIC pragma at run time.
--       When present, the Partial_DIC_Procedure of a type only checks DICs
--       associated with the partial (private) view of the type, and is invoked
--       by the full DIC_Procedure (which may check additional DICs associated
--       with the full view).

--       Note: the reason this is marked as a synthesized attribute is that the
--       way this is stored is as an element of the Subprograms_For_Type field.

--    Partial_Invariant_Procedure (synthesized)
--       Defined in types and subtypes. Set for private types when one or more
--       [class-wide] type invariants apply to them. Points to the entity for a
--       procedure which checks the invariant. This invariant procedure takes a
--       single argument of the given type, and returns if the invariant holds,
--       or raises exception Assertion_Error with an appropriate message if it
--       does not hold. This attribute is defined but always Empty for private
--       subtypes. This attribute is also set for the corresponding full type.
--
--       Note: the reason this is marked as a synthesized attribute is that the
--       way this is stored is as an element of the Subprograms_For_Type field.

--    Partial_Refinement_Constituents (synthesized)
--       Defined in abstract state entities. Returns the constituents that
--       refine the state in the current scope, which are allowed in a global
--       refinement in this scope. These consist of those constituents that are
--       abstract states with no or only partial refinement visible, and those
--       that are not themselves abstract states.

--    Partial_View_Has_Unknown_Discr
--       Present in all types. Set to Indicate that the partial view of a type
--       has unknown discriminants. A default initialization of an object of
--       the type does not require an invariant check (AI12-0133).

--    Pending_Access_Types
--       Defined in all types. Set for incomplete, private, Taft-amendment
--       types, and their corresponding full views. This list contains all
--       access types, both named and anonymous, declared between the partial
--       and the full view. The list is used by the finalization machinery to
--       ensure that the finalization masters of all pending access types are
--       fully initialized when the full view is frozen.

--    Postconditions_Proc
--       Obsolete field which can be removed once CodePeer is fixed ???

--    Predicate_Function (synthesized)
--       Defined in all types. Set for types for which (Has_Predicates is True)
--       and for which a predicate procedure has been built that tests that the
--       specified predicates are True. Contains the entity for the function
--       which takes a single argument of the given type (and sometimes an
--       additional Boolean parameter), and returns True if the predicate
--       holds and False if it does not.
--
--       Note: flag Has_Predicate does not imply that Predicate_Function is set
--       to a non-empty entity; this happens, for example, for itypes created
--       when instantiating generic units with private types with predicates.
--       However, if an explicit pragma Predicate or Predicate aspect is given
--       either for private or full type declaration then both Has_Predicates
--       and a non-empty Predicate_Function will be set on both the partial and
--       full views of the type.
--
--       Note: the reason this is marked as a synthesized attribute is that the
--       way this is stored is as an element of the Subprograms_For_Type field.

--    Predicate_Expression
--      Defined on functions. For the defining identifier of the subprogram
--      declaration (not of the subprogram body) of a predicate function,
--      yields the expression for the noninherited portion of the given
--      predicate (except in the case where the inherited portion is
--      non-empty and the non-inherited portion is empty, in which case the
--      expression for the inherited portion is returned). Otherwise yields
--      empty.

--    Predicated_Parent
--       Defined on itypes created by subtype indications, when the parent
--       subtype has predicates. The itype shares the Predicate_Function
--       of the predicated parent, but this function may not have been built
--       at the point the Itype is constructed, so this attribute allows its
--       retrieval at the point a predicate check needs to be generated.
--       The utility Predicate_Function takes this link into account.

--    Predicates_Ignored
--       Defined on all types. Indicates whether the subtype declaration is in
--       a context where Assertion_Policy is Ignore, in which case no checks
--       (static or dynamic) must be generated for objects of the type.

--    Prev_Entity
--       Defined in all entities. The entities of a scope are chained, and this
--       field is used as a backward pointer for this entity list - effectively
--       making the entity chain doubly-linked.

--    Primitive_Operations (synthesized)
--       Defined in concurrent types, tagged record types and subtypes, tagged
--       private types and tagged incomplete types. For concurrent types whose
--       Corresponding_Record_Type (CRT) is available, returns the list of
--       Direct_Primitive_Operations of its CRT; otherwise returns No_Elist.
--       For all the other types returns the Direct_Primitive_Operations.

--    Prival
--       Defined in private components of protected types. Refers to the entity
--       of the component renaming declaration generated inside protected
--       subprograms, entries or barrier functions.

--    Prival_Link
--       Defined in constants and variables which rename private components of
--       protected types. Set to the original private component.

--    Private_Dependents
--       Defined in private (sub)types. Records the subtypes of the private
--       type, derivations from it, and records and arrays with components
--       dependent on the type.
--
--       The subtypes are traversed when installing and deinstalling (the full
--       view of) a private type in order to ensure correct view of the
--       subtypes.
--
--       Used in similar fashion for incomplete types: holds list of subtypes
--       of these incomplete types that have discriminant constraints. The
--       full views of these subtypes are constructed when the full view of
--       the incomplete type is processed.

--       In addition, if the incomplete type is the designated type in an
--       access definition for an access parameter, the operation may be
--       a dispatching primitive operation, which is only known when the full
--       declaration of the type is seen. Subprograms that have such an
--       access parameter are also placed in the list of private_dependents.

--    Protected_Body_Subprogram
--       Defined in protected operations. References the entity for the
--       subprogram which implements the body of the operation.

--    Protected_Formal
--       Defined in formal parameters (in, in out and out parameters). Used
--       only for formals of protected operations. References corresponding
--       formal parameter in the unprotected version of the operation that
--       is created during expansion.

--    Protected_Subprogram
--       Defined in functions and procedures. Set for the pair of subprograms
--       which emulate the runtime semantics of a protected subprogram. Denotes
--       the entity of the origial protected subprogram.

--    Protection_Object
--       Applies to protected entries, entry families and subprograms. Denotes
--       the entity which is used to rename the _object component of protected
--       types.

--    Reachable
--       Defined in labels. The flag is set over the range of statements in
--       which a goto to that label is legal.

--    Receiving_Entry
--       Defined in procedures. Set for an internally generated procedure which
--       wraps the original statements of an accept alternative. Designates the
--       entity of the task entry being accepted.

--    Referenced
--       Defined in all entities. Set if the entity is referenced, except for
--       the case of an appearance of a simple variable that is not a renaming
--       as the left side of an assignment in which case Referenced_As_LHS is
--       set instead, or a similar appearance as an out parameter actual, in
--       which case Referenced_As_Out_Parameter is set.

--    Referenced_As_LHS
--       Defined in all entities. This flag is set instead of Referenced if a
--       simple variable that is not a renaming appears as the left side of an
--       assignment. The reason we distinguish this kind of reference is that
--       we have a separate warning for variables that are only assigned and
--       never read.

--    Referenced_As_Out_Parameter
--       Defined in all entities. This flag is set instead of Referenced if a
--       simple variable that is not a renaming appears as an actual for an out
--       formal. The reason we distinguish this kind of reference is that
--       we have a separate warning for variables that are only assigned and
--       never read, and out parameters are a special case.

--    Refinement_Constituents
--       Present in abstract state entities. Contains all the constituents that
--       refine the state, in other words, all the hidden states that appear in
--       the constituent_list of aspect/pragma Refined_State.

--    Register_Exception_Call
--       Defined in exception entities. When an exception is declared,
--       a call is expanded to Register_Exception. This field points to
--       the expanded N_Procedure_Call_Statement node for this call. It
--       is used for Import/Export_Exception processing to modify the
--       register call to make appropriate entries in the special tables
--       used for handling these pragmas at run time.

--    Related_Array_Object
--       Defined in array types and subtypes. Used only for the base type
--       and subtype created for an anonymous array object. Set to point
--       to the entity of the corresponding array object. Currently used
--       only for type-related error messages.

--    Related_Expression
--       Defined in variables, types and functions. When Set for internally
--       generated entities, it may be used to denote the source expression
--       whose elaboration created the variable declaration. If set, it is used
--       for generating clearer messages from CodePeer. It is used on source
--       entities that are variables in iterator specifications, to provide
--       a link to the container that is the domain of iteration. This allows
--       for better cross-reference information when the loop modifies elements
--       of the container, and suppresses spurious warnings.
--       Finally this node is used on functions specified via the Real_Literal
--       aspect, to denote the 2-parameter overloading, if found.
--
--       Shouldn't it also be used for the same purpose in errout? It seems
--       odd to have two mechanisms here???

--    Related_Instance
--       Defined in the wrapper packages created for subprogram instances.
--       The internal subprogram that implements the instance is inside the
--       wrapper package, but for debugging purposes its external symbol
--       must correspond to the name and scope of the related instance.

--    Related_Type
--       Defined in components, constants and variables. Set when there is an
--       associated dispatch table to point to entities containing primary or
--       secondary tags. Not set in the _tag component of record types.

--    Relative_Deadline_Variable [implementation base type only]
--       Defined in task type entities. This flag is set if a valid and
--       effective pragma Relative_Deadline applies to the base type. Points
--       to the entity for a variable that is created to hold the value given
--       in a Relative_Deadline pragma for a task type.

--    Renamed_Entity
--       Defined in exception, generic unit, package, and subprogram entities.
--       Set when the entity is defined by a renaming declaration. Denotes the
--       renamed entity, or transitively the ultimate renamed entity if there
--       is a chain of renaming declarations. Empty if no renaming.

--    Renamed_In_Spec
--       Defined in package entities. If a package renaming occurs within
--       a package spec, then this flag is set on the renamed package. The
--       purpose is to prevent a warning about unused entities in the renamed
--       package. Such a warning would be inappropriate since clients of the
--       package can see the entities in the package via the renaming.

--    Renamed_Object
--       Defined in components, constants, discriminants, formal parameters,
--       generic formals, loop parameters, and variables. Set to non-Empty if
--       the object was declared by a renaming declaration. For constants and
--       variables, the attribute references the tree node for the name of the
--       renamed object. For formal parameters, the field is used in inlining
--       and maps the entities of all formal parameters of a subprogram to the
--       entities of the corresponding actuals. For formals of a task entry,
--       the attribute denotes the local renaming that replaces the actual
--       within an accept statement. For all remaining cases (discriminants,
--       loop parameters) the field is Empty.

--    Requires_Overriding
--       Defined in all subprograms and entries. Set for subprograms that
--       require overriding as defined by RM-2005-3.9.3(6/2). Note that this
--       is True only for implicitly declared subprograms; it is not set on the
--       parent type's subprogram. See also Is_Abstract_Subprogram.

--    Return_Applies_To
--       Defined in E_Return_Statement. Points to the entity representing
--       the construct to which the return statement applies, as defined in
--       RM-6.5(4/2). Note that a (simple) return statement within an
--       extended_return_statement applies to the extended_return_statement,
--       even though it causes the whole function to return.
--       Also defined in special E_Block entities built as E_Return_Statement
--       for extended return statements and attached to the block statement
--       by Expand_N_Extended_Return_Statement before being turned into an
--       E_Block by semantic analysis.

--    Return_Present
--       Defined in function and generic function entities. Set if the
--       function contains a return statement (used for error checking).
--       This flag can also be set in procedure and generic procedure
--       entities (for convenience in setting it), but is only tested
--       for the function case.

--    Return_Statement
--       Defined in E_Variable. Set when Is_Return_Object is set, in which
--       case it points to the N_Simple_Return_Statement made from the
--       extended return statement.

--    Returns_By_Ref
--       Defined in subprogram type entities and functions. Set if a function
--       (or an access-to-function type) returns a result by reference, either
--       because the result is built in place, or its type is by-reference.

--    Reverse_Bit_Order [base type only]
--       Defined in all record type entities. Set if entity has a Bit_Order
--       aspect (set by an aspect clause or attribute definition clause) that
--       has reversed the order of bits from the default value. When this flag
--       is set, a component clause must specify a set of bits entirely within
--       a single storage unit (Ada 95) or within a single machine scalar (see
--       Ada 2005 AI-133), or must occupy an integral number of storage units.

--    Reverse_Storage_Order [base type only]
--       Defined in all record and array type entities. Set if entity has a
--       Scalar_Storage_Order aspect (set by an aspect clause or attribute
--       definition clause) that has reversed the order of storage elements
--       from the default value. When this flag is set for a record type,
--       the Bit_Order aspect must be set to the same value (either explicitly
--       or as the target default value).

--    Rewritten_For_C
--       Defined on functions that return a constrained array type, when
--       Modify_Tree_For_C is set. Indicates that a procedure with an extra
--       out parameter has been created for it, and calls must be rewritten as
--       calls to the new procedure.

--    RM_Size
--       Defined in all type and subtype entities. Contains the value of
--       type'Size as defined in the RM. See also the Esize field and
--       and the description on "Handling of Type'Size Values". A value
--       of zero in this field for a non-discrete type means that
--       the front end has not yet determined the size value. For the
--       case of a discrete type, this field is always set by the front
--       end and zero is a legitimate value for a type with one value.

--    Root_Type (synthesized)
--       Applies to all type entities. For class-wide types, returns the root
--       type of the class covered by the CW type, otherwise returns the
--       ultimate derivation ancestor of the given type. This function
--       preserves the view, i.e. the Root_Type of a partial view is the
--       partial view of the ultimate ancestor, the Root_Type of a full view
--       is the full view of the ultimate ancestor. Note that this function
--       does not correspond exactly to the use of root type in the RM, since
--       in the RM root type applies to a class of types, not to a type.

--    Scalar_Range
--       Defined in all scalar types (including modular types, where the
--       bounds are 0 .. modulus - 1). References a node in the tree that
--       contains the bounds for the range. Note that this information
--       could be obtained by rummaging around the tree, but it is more
--       convenient to have it immediately at hand in the entity. The
--       contents of Scalar_Range can either be an N_Subtype_Indication
--       node (with a constraint), a Range node, or an Integer_Type_Definition,
--       but not a simple subtype reference (a subtype is converted into a
--       explicit range).

--    Scale_Value
--       Defined in decimal fixed-point types and subtypes. This holds the
--       value of the Scale attribute for the type, i.e. the scale of the type
--       defined as the integer N such that the delta is equal to 10.0**(-N).
--       Note that, if Scale_Value is positive, then it is equal to Aft_Value.

--    Scope
--       Defined in all entities. Points to the entity for the scope (block,
--       loop, subprogram, package etc.) in which the entity is declared.
--       Since this field is in the base part of the entity node, the access
--       routines for this field are in Sinfo. Note that for a child unit,
--       the Scope will be the parent package, and for a root library unit,
--       the Scope will be Standard.

--    Scope_Depth (synthesized)
--       Applies to program units, blocks, loops, return statements,
--       concurrent types, private types and entries, and also to record types,
--       i.e. to any entity that can appear on the scope stack. Yields the
--       scope depth value, which for those entities other than records is
--       simply the scope depth value, for record entities, it is the
--       Scope_Depth of the record scope.

--    Scope_Depth_Value
--       Defined in program units, blocks, loops, return statements,
--       concurrent types, private types and entries.
--       Indicates the number of scopes that statically enclose the declaration
--       of the unit or type. Library units have a depth of zero. Note that
--       record types can act as scopes but do NOT have this field set (see
--       Scope_Depth above). Queries should normally be via Scope_Depth,
--       and not call Scope_Depth_Value directly.

--    Scope_Depth_Set (synthesized)
--       Applies to a special predicate function that returns a Boolean value
--       indicating whether or not the Scope_Depth field has been set. It is
--       needed, since returns an invalid value in this case.

--    Sec_Stack_Needed_For_Return
--       Defined in scope entities (blocks, entries, entry families, functions,
--       and procedures). Set to True when secondary stack is used to hold the
--       returned value of a function and thus should not be released on scope
--       exit.

--    Shared_Var_Procs_Instance
--       Defined in variables. Set non-Empty only if Is_Shared_Passive is
--       set, in which case this is the entity for the associated instance of
--       System.Shared_Storage.Shared_Var_Procs. See Exp_Smem for full details.

--    Size_Check_Code
--       Defined in constants and variables. Normally Empty. Set if code is
--       generated to check the size of the object. This field is used to
--       suppress this code if a subsequent address clause is encountered.

--    Size_Clause (synthesized)
--       Applies to all entities. If a size or value size clause is present in
--       the rep item chain for an entity then that attribute definition clause
--       is returned. Otherwise Size_Clause returns Empty. Usually this is only
--       meaningful if the flag Has_Size_Clause is set. This is because when
--       the representation item chain is copied for a derived type, it can
--       inherit a size clause that is not applicable to the entity.

--    Size_Depends_On_Discriminant
--       Defined in all entities for types and subtypes. Indicates that the
--       size of the type depends on the value of one or more discriminants.
--       Currently, this flag is only set for arrays which have one or more
--       bounds depending on a discriminant value.

--    Size_Known_At_Compile_Time
--       Defined in all entities for types and subtypes. Indicates that the
--       size of objects of the type is known at compile time. This flag is
--       used to optimize some generated code sequences, and also to enable
--       some error checks (e.g. disallowing component clauses on variable
--       length objects). It is set conservatively (i.e. if it is True, the
--       size is certainly known at compile time, if it is False, then the
--       size may or may not be known at compile time, but the code will
--       assume that it is not known). Note that the value may be known only
--       to the back end, so the fact that this flag is set does not mean that
--       the front end can access the value.

--    Small_Value
--       Defined in fixed point types. Points to the universal real for the
--       Small of the type, either as given in a representation clause, or
--       as computed (as a power of two) by the compiler.

--    SPARK_Aux_Pragma
--       Present in concurrent type, [generic] package spec and package body
--       entities. For concurrent types and package specs it refers to the
--       SPARK mode setting for the private part. This field points to the
--       N_Pragma node that either appears in the private part or is inherited
--       from the enclosing context. For package bodies, it refers to the SPARK
--       mode of the elaboration sequence after the BEGIN. The fields points to
--       the N_Pragma node that either appears in the statement sequence or is
--       inherited from the enclosing context. In all cases, if the pragma is
--       inherited, then the SPARK_Aux_Pragma_Inherited flag is set.

--    SPARK_Aux_Pragma_Inherited
--       Present in concurrent type, [generic] package spec and package body
--       entities. Set if the SPARK_Aux_Pragma field points to a pragma that is
--       inherited, rather than a local one.

--    SPARK_Pragma
--       Present in the following entities:
--
--         abstract states
--         constants
--         entries
--         operators
--         [generic] packages
--         package bodies
--         [generic] subprograms
--         subprogram bodies
--         variables
--         types
--
--       Points to the N_Pragma node that applies to the initial declaration or
--       body. This is either set by a local SPARK_Mode pragma or is inherited
--       from the context (from an outer scope for the spec case or from the
--       spec for the body case). In the case where the attribute is inherited,
--       flag SPARK_Pragma_Inherited is set. Empty if no SPARK_Mode pragma is
--       applicable.

--    SPARK_Pragma_Inherited
--       Present in the following entities:
--
--         abstract states
--         constants
--         entries
--         operators
--         [generic] packages
--         package bodies
--         [generic] subprograms
--         subprogram bodies
--         variables
--         types
--
--       Set if the SPARK_Pragma attribute points to an inherited pragma rather
--       than a local one.

--    Spec_Entity
--       Defined in package body entities. Points to corresponding package
--       spec entity. Also defined in subprogram body parameters in the
--       case where there is a separate spec, where this field references
--       the corresponding parameter entities in the spec.

--    SSO_Set_High_By_Default [base type only]
--       Defined for record and array types. Set in the base type if a pragma
--       Default_Scalar_Storage_Order (High_Order_First) was active at the time
--       the record or array was declared and therefore applies to it.

--    SSO_Set_Low_By_Default [base type only]
--       Defined for record and array types. Set in the base type if a pragma
--       Default_Scalar_Storage_Order (High_Order_First) was active at the time
--       the record or array was declared and therefore applies to it.

--    Static_Call_Helper
--       Defined on subprogram entities. Set if the subprogram has class-wide
--       preconditions. Denotes the helper that evaluates at runtime the
--       class-wide preconditions performing static calls.

--    Static_Discrete_Predicate
--       Defined in discrete types/subtypes with static predicates (with the
--       two flags Has_Predicates and Has_Static_Predicate set). Set if the
--       type/subtype has a static predicate. Points to a list of expression
--       and N_Range nodes that represent the predicate in canonical form. The
--       canonical form has entries sorted in ascending order, with duplicates
--       eliminated, and adjacent ranges coalesced, so that there is always a
--       gap in the values between successive entries. The entries in this list
--       are fully analyzed and typed with the base type of the subtype. Note
--       that all entries are static and have values within the subtype range.

--    Static_Elaboration_Desired
--       Defined in library-level packages. Set by the pragma of the same
--       name, to indicate that static initialization must be attempted for
--       all types declared in the package, and that a warning must be emitted
--       for those types to which static initialization is not available.

--    Static_Initialization
--       Defined in initialization procedures for types whose objects can be
--       initialized statically. The value of this attribute is a positional
--       aggregate whose components are compile-time static values. Used
--       when available in object declarations to eliminate the call to the
--       initialization procedure, and to minimize elaboration code. Note:
--       This attribute uses the same field as Overridden_Operation, which is
--       irrelevant in init_procs.

--    Static_Real_Or_String_Predicate
--       Defined in real types/subtypes with static predicates (with the two
--       flags Has_Predicates and Has_Static_Predicate set). Set if the type
--       or subtype has a static predicate. Points to the return expression
--       of the predicate function. This is the original expression given as
--       the predicate except that occurrences of the type are replaced by
--       occurrences of the formal parameter of the predicate function (note
--       that the spec of this function including this formal parameter name
--       is available from the Subprograms_For_Type field; it can be accessed
--       as Predicate_Function (typ)). Also, in the case where a predicate is
--       inherited, the expression is of the form:
--
--         xxxPredicate (typ2 (ent)) AND THEN expression
--
--       where typ2 is the type from which the predicate is inherited, ent is
--       the entity for the current predicate function, and xxxPredicate is the
--       inherited predicate (from typ2). Finally for a predicate that inherits
--       from another predicate but does not add a predicate of its own, the
--       expression may consist of the above xxxPredicate call on its own.

--    Status_Flag_Or_Transient_Decl
--       Defined in constant, loop, and variable entities. Applies to objects
--       that require special treatment by the finalization machinery, such as
--       extended return results, IF and CASE expression results, and objects
--       inside N_Expression_With_Actions nodes. The attribute contains the
--       entity of a flag which specifies particular behavior over a region of
--       code or the declaration of a "hook" object.
--       In which case is it a flag, or a hook object???

--    Storage_Size_Variable [implementation base type only]
--       Defined in access types and task type entities. This flag is set
--       if a valid and effective pragma Storage_Size applies to the base
--       type. Points to the entity for a variable that is created to
--       hold the value given in a Storage_Size pragma for an access
--       collection or a task type. Note that in the access type case,
--       this field is defined only in the root type (since derived types
--       share the same storage pool).

--    Stored_Constraint
--       Defined in entities that can have discriminants (concurrent types
--       subtypes, record types and subtypes, private types and subtypes,
--       limited private types and subtypes and incomplete types). Points
--       to an element list containing the expressions for each of the
--       stored discriminants for the record (sub)type.

--    Stores_Attribute_Old_Prefix
--       Defined in constants, variables, and types which are created during
--       expansion in order to save the value of attribute 'Old's prefix.

--    Strict_Alignment [implementation base type only]
--       Defined in all type entities. Indicates that the type is by-reference
--       or contains an aliased part. This forbids packing a component of this
--       type tighter than the alignment and size of the type, as specified by
--       RM 13.2(7) modified by AI12-001 as a Binding Interpretation.

--    String_Literal_Length
--       Defined in string literal subtypes (which are created to correspond
--       to string literals in the program). Contains the length of the string
--       literal.

--    String_Literal_Low_Bound
--       Defined in string literal subtypes (which are created to correspond
--       to string literals in the program). Contains an expression whose
--       value represents the low bound of the literal. This is a copy of
--       the low bound of the applicable index constraint if there is one,
--       or a copy of the low bound of the index base type if not.

--    Subprograms_For_Type
--       Defined in all types. The list may contain the entities of the default
--       initial condition procedure, invariant procedure, and the two versions
--       of the predicate function.
--
--       Historical note: This attribute used to be a direct linked list of
--       entities rather than an Elist. The Elist allows greater flexibility
--       in inheritance of subprograms between views of the same type.

--    Subps_Index
--       Present in subprogram entities. Set if the subprogram contains nested
--       subprograms, or is a subprogram nested within such a subprogram. Holds
--       the index in the Exp_Unst.Subps table for the subprogram. Note that
--       for the outer level subprogram, this is the starting index in the Subp
--       table for the entries for this subprogram.

--    Suppress_Elaboration_Warnings
--       NOTE: this flag is relevant only for the legacy ABE mechanism and
--       should not be used outside of that context.
--
--       Defined in all entities, can be set only for subprogram entities and
--       for variables. If this flag is set then Sem_Elab will not generate
--       elaboration warnings for the subprogram or variable. Suppression of
--       such warnings is automatic for subprograms for which elaboration
--       checks are suppressed (without the need to set this flag), but the
--       flag is also set for various internal entities (such as init procs)
--       which are known not to generate any possible access before elaboration
--       and it is set on variables when a warning is given to avoid multiple
--       elaboration warnings for the same variable.

--    Suppress_Initialization
--       Defined in all variable, type and subtype entities. If set for a base
--       type, then the generation of initialization procedures is suppressed
--       for the type. Any other implicit initialization (e.g. from the use of
--       pragma Initialize_Scalars) is also suppressed if this flag is set for
--       either the subtype in question, or for the base type. For variables,
--       this flag suppresses all implicit initialization for the object, even
--       if the type would normally require initialization. Set by use of
--       pragma Suppress_Initialization and also for internal entities where
--       we know that no initialization is required. For example, enumeration
--       image table entities set it.

--    Suppress_Style_Checks
--       Defined in all entities. Suppresses any style checks specifically
--       associated with the given entity if set.

--    Suppress_Value_Tracking_On_Call
--       Defined in all entities. Set in a scope entity if value tracking is to
--       be suppressed on any call within the scope. Used when an access to a
--       local subprogram is computed, to deal with the possibility that this
--       value may be passed around, and if used, may clobber a local variable.

--    Task_Body_Procedure
--       Defined in task types and subtypes. Points to the entity for the task
--       task body procedure (as further described in Exp_Ch9, task bodies are
--       expanded into procedures). A convenient function to retrieve this
--       field is Sem_Util.Get_Task_Body_Procedure.
--
--       The last sentence is odd??? Why not have Task_Body_Procedure go to the
--       Underlying_Type of the Root_Type???

--    Thunk_Entity
--       Defined in functions and procedures which have been classified as
--       Is_Thunk. Set to the target entity called by the thunk.

--    Treat_As_Volatile
--       Defined in all type entities, and also in constants, components and
--       variables. Set if this entity is to be treated as volatile for code
--       generation purposes. Always set if Is_Volatile is set, but can also
--       be set as a result of situations (such as address overlays) where
--       the front end wishes to force volatile handling to inhibit aliasing
--       optimization which might be legally ok, but is undesirable. Note
--       that the backend always tests this flag rather than Is_Volatile.
--       The front end tests Is_Volatile if it is concerned with legality
--       checks associated with declared volatile variables, but if the test
--       is for the purposes of suppressing optimizations, then the front
--       end should test Treat_As_Volatile rather than Is_Volatile.
--
--       Note: before testing Treat_As_Volatile, consider whether it would
--       be more appropriate to use Exp_Util.Is_Volatile_Reference instead,
--       which catches more cases of volatile references.

--    Type_High_Bound (synthesized)
--       Applies to scalar types. Returns the tree node (Node_Id) that contains
--       the high bound of a scalar type. The returned value is literal for a
--       base type, but may be an expression in the case of scalar type with
--       dynamic bounds.

--    Type_Low_Bound (synthesized)
--       Applies to scalar types. Returns the tree node (Node_Id) that contains
--       the low bound of a scalar type. The returned value is literal for a
--       base type, but may be an expression in the case of scalar type with
--       dynamic bounds.

--    Underlying_Full_View
--       Defined in private subtypes that are the completion of other private
--       types, or in private types that are derived from private subtypes. If
--       the full view of a private type T is derived from another private type
--       with discriminants Td, the full view of T is also private, and there
--       is no way to attach to it a further full view that would convey the
--       structure of T to the backend. The Underlying_Full_View is an
--       attribute of the full view that is a subtype of Td with the same
--       constraint as the declaration for T. The declaration for this subtype
--       is built at the point of the declaration of T, either as completion,
--       or as a subtype declaration where the base type is private and has a
--       private completion. If Td is already constrained, then its full view
--       can serve directly as the full view of T.

--    Underlying_Record_View
--       Defined in record types. Set for record types that are extensions of
--       types with unknown discriminants, and also set for internally built
--       underlying record views to reference its original record type. Record
--       types that are extensions of types with unknown discriminants do not
--       have a completion, but they cannot be used without having some
--       discriminated view at hand. This view is a record type with the same
--       structure, whose parent type is the full view of the parent in the
--       original type extension.

--    Underlying_Type (synthesized)
--       Applies to all entities. This is the identity function except in the
--       case where it is applied to an incomplete or private type, in which
--       case it is the underlying type of the type declared by the completion,
--       or Empty if the completion has not yet been encountered and analyzed.
--
--       Note: the reason this attribute applies to all entities, and not just
--       types, is to legitimize code where Underlying_Type is applied to an
--       entity which may or may not be a type, with the intent that if it is a
--       type, its underlying type is taken.
--
--       Note also that the value of this attribute is interesting only after
--       the full view of the parent type has been processed. If the parent
--       type is declared in an enclosing package, the attribute will be non-
--       trivial only after the full view of the type has been analyzed.

--    Universal_Aliasing [implementation base type only]
--       Defined in all type entities. Set to direct the back-end to avoid
--       any optimizations based on type-based alias analysis for this type.
--       Indicates that objects of this type can alias objects of any other
--       types, which guarantees that any objects can be referenced through
--       access types designating this type safely, whatever the actual type
--       of these objects. In other words, the effect is as though access
--       types designating this type were subject to No_Strict_Aliasing.

--    Unset_Reference
--       Defined in variables and out parameters. This is normally Empty. It
--       is set to point to an identifier that represents a reference to the
--       entity before any value has been set. Only the first such reference
--       is identified. This field is used to generate a warning message if
--       necessary (see Sem_Warn.Check_Unset_Reference).

--    Used_As_Generic_Actual
--       Defined in all entities, set if the entity is used as an argument to
--       a generic instantiation. Used to tune certain warning messages, and
--       in checking type conformance within an instantiation that involves
--       incomplete formal and actual types.

--    Uses_Lock_Free
--       Defined in protected type entities. Set to True when the Lock Free
--       implementation is used for the protected type. This implementation is
--       based on atomic transactions and doesn't require anymore the use of
--       Protection object (see System.Tasking.Protected_Objects).

--    Uses_Sec_Stack
--       Defined in scope entities (blocks, entries, entry families, functions,
--       loops, and procedures). Set to True when the secondary stack is used
--       in this scope and must be released on exit unless flag
--       Sec_Stack_Needed_For_Return is set.

--    Validated_Object
--       Defined in variables. Contains the object whose value is captured by
--       the variable for validity check purposes.

--    Warnings_Off
--       Defined in all entities. Set if a pragma Warnings (Off, entity-name)
--       is used to suppress warnings for a given entity. It is also used by
--       the compiler in some situations to kill spurious warnings. Note that
--       clients should generally not test this flag directly, but instead
--       use function Has_Warnings_Off.

--    Warnings_Off_Used
--       Defined in all entities. Can only be set if Warnings_Off is set. If
--       set indicates that a warning was suppressed by the Warnings_Off flag,
--       and Unmodified/Unreferenced would not have suppressed the warning.

--    Warnings_Off_Used_Unmodified
--       Defined in all entities. Can only be set if Warnings_Off is set and
--       Has_Pragma_Unmodified is not set. If set indicates that a warning was
--       suppressed by the Warnings_Off status but that pragma Unmodified
--       would also have suppressed the warning.

--    Warnings_Off_Used_Unreferenced
--       Defined in all entities. Can only be set if Warnings_Off is set and
--       Has_Pragma_Unreferenced is not set. If set indicates that a warning
--       was suppressed by the Warnings_Off status but that pragma Unreferenced
--       would also have suppressed the warning.

--    Was_Hidden
--       Defined in all entities. Used to save the value of the Is_Hidden
--       attribute when the limited-view is installed (Ada 2005: AI-217).

--    Wrapped_Entity
--       Defined in functions and procedures which have been classified as
--       Is_Primitive_Wrapper. Set to the entity being wrapper.

--    Wrapped_Statements
--       Defined in functions, procedures, entries, and entry families. Refers
--       to the entity of the _Wrapped_Statements procedure which gets
--       generated as part of the expansion of contracts and postconditions
--       and contains its enclosing subprogram's original source declarations
--       and statements.

--    LSP_Subprogram
--       Defined in subprogram entities. Set on wrappers created to handle
--       inherited class-wide pre/post conditions that call overridden
--       primitives. It references the parent primitive that has the
--       class-wide pre/post conditions.

---------------------------
-- Renaming and Aliasing --
---------------------------

--  Several entity attributes relate to renaming constructs, and to the use of
--  different names to refer to the same entity. The following is a summary of
--  these constructs and their preferred uses.

--  There are three related attributes:

--    Renamed_Entity
--    Renamed_Object
--    Alias

--  These are implemented in Einfo.Utils as renamings of the Renamed_Or_Alias
--  field. They are semantically related, and have the following intended uses:

--  a) Renamed_Entity applies to entities in renaming declarations that rename
--  an entity, so the value of the attribute IS an entity. This applies to
--  generic renamings, package renamings, exception renamings, and subprogram
--  renamings that rename a subprogram (rather than an attribute, an entry, a
--  protected operation, etc).

--  b) Alias applies to overloadable entities, and the value is an overloadable
--  entity. So this is a subset of the previous one. We use the term Alias to
--  cover both renamings and inherited operations, because both cases are
--  handled in the same way when expanding a call. Namely the Alias of a given
--  subprogram is the subprogram that will actually be called.

--  Both a) and b) are set transitively, so that in fact it is not necessary to
--  traverse chains of renamings when looking for the original entity: it's
--  there in one step (this is done when analyzing renaming declarations other
--  than object renamings in sem_ch8).

--  c) Renamed_Object applies to constants and variables. Given that the name
--  in an object renaming declaration is not necessarily an entity name, the
--  value of the attribute is the tree for that name, eg AR (1).Comp. The case
--  when that name is in fact an entity is not handled specially. This is why
--  in a few cases we need to use a loop to trace a chain of object renamings
--  where all of them happen to be entities. So:

--    X : Integer;
--    Y : Integer renames X;   -- renamed object is the identifier X
--    Z : Integer renames Y;   -- renamed object is the identifier Y

--  The front-end does not store explicitly the fact that Z renames X.

------------------
-- Access Kinds --
------------------

--  The following entity kinds are introduced by the corresponding type
--  definitions:

--    E_Access_Type,
--    E_General_Access_Type,
--    E_Anonymous_Access_Type

--    E_Access_Subprogram_Type,
--    E_Anonymous_Access_Subprogram_Type,

--    E_Access_Protected_Subprogram_Type,
--    E_Anonymous_Access_Protected_Subprogram_Type

--  E_Access_Subtype is for an access subtype created by a subtype declaration

--  In addition, we define the kind E_Allocator_Type to label allocators.
--  This is because special resolution rules apply to this construct.
--  Eventually the constructs are labeled with the access type imposed by
--  the context. The backend should never see types with this Ekind.

--  Similarly, we define the kind E_Access_Attribute_Type as the initial
--  kind associated with an access attribute whose prefix is an object.
--  After resolution, a specific access type will be established instead
--  as determined by the context. Note that, for the case of an access
--  attribute whose prefix is a subprogram, we build a corresponding type
--  with E_Access_Subprogram_Type or E_Access_Protected_Subprogram_Type kind
--  but whose designated type is the subprogram itself, instead of a regular
--  E_Subprogram_Type entity.

   --------------------------------------------------------
   -- Description of Defined Attributes for Entity_Kinds --
   --------------------------------------------------------

   --  For each enumeration value defined in Entity_Kind we list all the
   --  attributes defined in Einfo which can legally be applied to an entity
   --  of that kind. The implementation of the attribute functions (and for
   --  non-synthesized attributes, of the corresponding set procedures) are
   --  in the Einfo body.

   --  The following attributes are defined in all entities

   --    Ekind                               (Ekind)

   --    Chars
   --    Next_Entity
   --    Scope
   --    Homonym
   --    Etype
   --    First_Rep_Item
   --    Freeze_Node
   --    Prev_Entity
   --    Associated_Entity

   --    Address_Taken
   --    Can_Never_Be_Null
   --    Checks_May_Be_Suppressed
   --    Debug_Info_Off
   --    Has_Convention_Pragma
   --    Has_Delayed_Aspects
   --    Has_Delayed_Freeze
   --    Has_Fully_Qualified_Name
   --    Has_Gigi_Rep_Item
   --    Has_Homonym
   --    Has_Pragma_Elaborate_Body
   --    Has_Pragma_Inline
   --    Has_Pragma_Inline_Always
   --    Has_Pragma_No_Inline
   --    Has_Pragma_Pure
   --    Has_Pragma_Pure_Function
   --    Has_Pragma_Thread_Local_Storage
   --    Has_Pragma_Unmodified
   --    Has_Pragma_Unreferenced
   --    Has_Pragma_Unused
   --    Has_Private_Declaration
   --    Has_Qualified_Name
   --    Has_Stream_Size_Clause
   --    Has_Unknown_Discriminants
   --    Has_Xref_Entry
   --    In_Private_Part
   --    Is_Ada_2005_Only
   --    Is_Ada_2012_Only
   --    Is_Ada_2022_Only
   --    Is_Bit_Packed_Array                  (base type only)
   --    Is_Aliased
   --    Is_Character_Type
   --    Is_Checked_Ghost_Entity
   --    Is_Child_Unit
   --    Is_Compilation_Unit
   --    Is_Descendant_Of_Address
   --    Is_Discrim_SO_Function
   --    Is_Discriminant_Check_Function
   --    Is_Dispatch_Table_Entity
   --    Is_Dispatch_Table_Wrapper
   --    Is_Dispatching_Operation
   --    Is_Entry_Formal
   --    Is_Exported
   --    Is_First_Subtype
   --    Is_Formal_Subprogram
   --    Is_Generic_Instance
   --    Is_Generic_Type
   --    Is_Hidden
   --    Is_Hidden_Open_Scope
   --    Is_Ignored_Ghost_Entity
   --    Is_Immediately_Visible
   --    Is_Implementation_Defined
   --    Is_Imported
   --    Is_Inlined
   --    Is_Internal
   --    Is_Itype
   --    Is_Known_Non_Null
   --    Is_Known_Null
   --    Is_Known_Valid
   --    Is_Limited_Composite
   --    Is_Limited_Record
   --    Is_Loop_Parameter
   --    Is_Obsolescent
   --    Is_Package_Body_Entity
   --    Is_Packed_Array_Impl_Type
   --    Is_Potentially_Use_Visible
   --    Is_Preelaborated
   --    Is_Primitive_Wrapper
   --    Is_Public
   --    Is_Pure
   --    Is_Remote_Call_Interface
   --    Is_Remote_Types
   --    Is_Renaming_Of_Object
   --    Is_Shared_Passive
   --    Is_Statically_Allocated
   --    Is_Static_Type
   --    Is_Tagged_Type
   --    Is_Thunk
   --    Is_Trivial_Subprogram
   --    Is_Unchecked_Union
   --    Is_Unimplemented
   --    Is_Visible_Formal
   --    Kill_Elaboration_Checks
   --    Kill_Range_Checks
   --    Low_Bound_Tested
   --    Materialize_Entity
   --    Needs_Debug_Info
   --    Never_Set_In_Source
   --    No_Return
   --    Overlays_Constant
   --    Referenced
   --    Referenced_As_LHS
   --    Referenced_As_Out_Parameter
   --    Suppress_Elaboration_Warnings
   --    Suppress_Style_Checks
   --    Suppress_Value_Tracking_On_Call
   --    Used_As_Generic_Actual
   --    Warnings_Off
   --    Warnings_Off_Used
   --    Warnings_Off_Used_Unmodified
   --    Warnings_Off_Used_Unreferenced
   --    Was_Hidden

   --    Declaration_Node                    (synth)
   --    Has_Foreign_Convention              (synth)
   --    Is_Dynamic_Scope                    (synth)
   --    Is_Ghost_Entity                     (synth)
   --    Is_Standard_Character_Type          (synth)
   --    Is_Standard_String_Type             (synth)
   --    Underlying_Type                     (synth)
   --    all classification attributes       (synth)

   --  The following list of access functions applies to all entities for
   --  types and subtypes. References to this list appear subsequently as
   --  "(plus type attributes)" for each appropriate Entity_Kind.

   --    Associated_Node_For_Itype
   --    Class_Wide_Type
   --    Full_View
   --    Esize
   --    RM_Size
   --    Alignment
   --    Pending_Access_Types
   --    Related_Expression
   --    Current_Use_Clause
   --    Subprograms_For_Type
   --    Derived_Type_Link
   --    No_Tagged_Streams_Pragma
   --    Linker_Section_Pragma
   --    SPARK_Pragma

   --    Depends_On_Private
   --    Disable_Controlled
   --    Discard_Names
   --    Finalize_Storage_Only                (base type only)
   --    From_Limited_With
   --    Has_Aliased_Components               (base type only)
   --    Has_Alignment_Clause
   --    Has_Atomic_Components                (base type only)
   --    Has_Completion_In_Body
   --    Has_Complex_Representation           (base type only)
   --    Has_Constrained_Partial_View
   --    Has_Controlled_Component             (base type only)
   --    Has_Default_Aspect                   (base type only)
   --    Has_Delayed_Rep_Aspects
   --    Has_Discriminants
   --    Has_Dynamic_Predicate_Aspect
   --    Has_Independent_Components           (base type only)
   --    Has_Inheritable_Invariants           (base type only)
   --    Has_Inherited_DIC                    (base type only)
   --    Has_Inherited_Invariants             (base type only)
   --    Has_Non_Standard_Rep                 (base type only)
   --    Has_Object_Size_Clause
   --    Has_Own_DIC                          (base type only)
   --    Has_Own_Invariants                   (base type only)
   --    Has_Pragma_Preelab_Init
   --    Has_Pragma_Unreferenced_Objects
   --    Has_Predicates
   --    Has_Primitive_Operations             (base type only)
   --    Has_Protected                        (base type only)
   --    Has_Size_Clause
   --    Has_Specified_Layout                 (base type only)
   --    Has_Specified_Stream_Input
   --    Has_Specified_Stream_Output
   --    Has_Specified_Stream_Read
   --    Has_Specified_Stream_Write
   --    Has_Static_Predicate
   --    Has_Static_Predicate_Aspect
   --    Has_Task                             (base type only)
   --    Has_Timing_Event                     (base type only)
   --    Has_Unchecked_Union                  (base type only)
   --    Has_Volatile_Components              (base type only)
   --    In_Use
   --    Is_Abstract_Type
   --    Is_Asynchronous
   --    Is_Atomic
   --    Is_Constr_Subt_For_U_Nominal
   --    Is_Constr_Subt_For_UN_Aliased
   --    Is_Controlled_Active                 (base type only)
   --    Is_Eliminated
   --    Is_Frozen
   --    Is_Generic_Actual_Type
   --    Is_Independent
   --    Is_Non_Static_Subtype
   --    Is_Packed                            (base type only)
   --    Is_Private_Composite
   --    Is_RACW_Stub_Type
   --    Is_Unsigned_Type
   --    Is_Volatile
   --    Is_Volatile_Full_Access
   --    Itype_Printed                        (itypes only)
   --    Known_To_Have_Preelab_Init
   --    May_Inherit_Delayed_Rep_Aspects
   --    Must_Be_On_Byte_Boundary
   --    Must_Have_Preelab_Init
   --    Optimize_Alignment_Space
   --    Optimize_Alignment_Time
   --    Partial_View_Has_Unknown_Discr
   --    Size_Depends_On_Discriminant
   --    Size_Known_At_Compile_Time
   --    SPARK_Pragma_Inherited
   --    Strict_Alignment                     (base type only)
   --    Suppress_Initialization
   --    Treat_As_Volatile
   --    Universal_Aliasing                   (impl base type only)

   --    Alignment_Clause                    (synth)
   --    Base_Type                           (synth)
   --    DIC_Procedure                       (synth)
   --    Has_DIC                             (synth)
   --    Has_Invariants                      (synth)
   --    Implementation_Base_Type            (synth)
   --    Invariant_Procedure                 (synth)
   --    Is_Access_Protected_Subprogram_Type (synth)
   --    Is_Full_Access                      (synth)
   --    Is_Controlled                       (synth)
   --    Object_Size_Clause                  (synth)
   --    Partial_DIC_Procedure               (synth)
   --    Partial_Invariant_Procedure         (synth)
   --    Predicate_Function                  (synth)
   --    Root_Type                           (synth)
   --    Size_Clause                         (synth)

   ------------------------------------------
   -- Applicable attributes by entity kind --
   ------------------------------------------

   --  In the conversion to variable-sized nodes and entities, a number of
   --  discrepancies were noticed. They are documented in comments, and marked
   --  with "$$$".

   --  E_Abstract_State
   --    Refinement_Constituents
   --    Part_Of_Constituents
   --    Body_References
   --    Non_Limited_View
   --    Encapsulating_State
   --    SPARK_Pragma
   --    From_Limited_With
   --    Has_Partial_Visible_Refinement
   --    Has_Visible_Refinement
   --    SPARK_Pragma_Inherited
   --    First_Entity $$$
   --    Has_Non_Limited_View                (synth)
   --    Has_Non_Null_Visible_Refinement     (synth)
   --    Has_Null_Visible_Refinement         (synth)
   --    Is_External_State                   (synth)
   --    Is_Null_State                       (synth)
   --    Is_Relaxed_Initialization_State     (synth)
   --    Is_Synchronized_State               (synth)
   --    Partial_Refinement_Constituents     (synth)

   --  E_Access_Protected_Subprogram_Type
   --    Equivalent_Type
   --    Directly_Designated_Type
   --    Needs_No_Actuals
   --    Can_Use_Internal_Rep
   --    (plus type attributes)

   --  E_Access_Subprogram_Type
   --    Equivalent_Type                       (remote types only)
   --    Directly_Designated_Type
   --    Needs_No_Actuals
   --    Original_Access_Type
   --    Can_Use_Internal_Rep
   --    Needs_Activation_Record
   --    Associated_Storage_Pool $$$
   --    Interface_Name $$$
   --    (plus type attributes)

   --  E_Access_Type
   --  E_Access_Subtype
   --    Direct_Primitive_Operations $$$ type
   --    Master_Id
   --    Directly_Designated_Type
   --    Associated_Storage_Pool               (base type only)
   --    Finalization_Master                   (base type only)
   --    Storage_Size_Variable                 (base type only)
   --    Has_Pragma_Controlled                 (base type only)
   --    Has_Storage_Size_Clause               (base type only)
   --    Is_Access_Constant
   --    Is_Local_Anonymous_Access
   --    Is_Pure_Unit_Access_Type
   --    No_Pool_Assigned                     (base type only)
   --    No_Strict_Aliasing                   (base type only)
   --    Is_Param_Block_Component_Type        (base type only)
   --    (plus type attributes)

   --  E_Access_Attribute_Type
   --    Renamed_Entity $$$
   --    Directly_Designated_Type
   --    (plus type attributes)

   --  E_Allocator_Type
   --    Directly_Designated_Type
   --    Associated_Storage_Pool $$$
   --    (plus type attributes)

   --  E_Anonymous_Access_Subprogram_Type
   --  E_Anonymous_Access_Protected_Subprogram_Type
   --    Interface_Name $$$ E_Anonymous_Access_Subprogram_Type
   --    Directly_Designated_Type
   --    Storage_Size_Variable                 is this needed ???
   --    Can_Use_Internal_Rep
   --    Needs_Activation_Record
   --    (plus type attributes)

   --  E_Anonymous_Access_Type
   --    Directly_Designated_Type
   --    Finalization_Master
   --    Storage_Size_Variable                 is this needed ???
   --    Associated_Storage_Pool $$$
   --    (plus type attributes)

   --  E_Array_Type
   --  E_Array_Subtype
   --    First_Entity $$$
   --    Direct_Primitive_Operations $$$ subtype
   --    Renamed_Object $$$ E_Array_Subtype
   --    First_Index
   --    Default_Aspect_Component_Value        (base type only)
   --    Component_Type                        (base type only)
   --    Original_Array_Type
   --    Component_Size                        (base type only)
   --    Packed_Array_Impl_Type
   --    Related_Array_Object
   --    Predicated_Parent                     (subtype only)
   --    Component_Alignment                   (special)  (base type only)
   --    Has_Component_Size_Clause             (base type only)
   --    Has_Pragma_Pack                       (impl base type only)
   --    Is_Constrained
   --    Reverse_Storage_Order                 (base type only)
   --    SSO_Set_High_By_Default               (base type only)
   --    SSO_Set_Low_By_Default                (base type only)
   --    Next_Index                            (synth)
   --    Number_Dimensions                     (synth)
   --    (plus type attributes)

   --  E_Block
   --    Renamed_Entity $$$
   --    Renamed_Object $$$
   --    Return_Applies_To
   --    Block_Node
   --    First_Entity
   --    Last_Entity
   --    Scope_Depth_Value
   --    Entry_Cancel_Parameter
   --    Contains_Ignored_Ghost_Code
   --    Delay_Cleanups
   --    Discard_Names
   --    Has_Master_Entity
   --    Has_Nested_Block_With_Handler
   --    Is_Exception_Handler
   --    Sec_Stack_Needed_For_Return
   --    Uses_Sec_Stack
   --    Scope_Depth                         (synth)

   --  E_Class_Wide_Type
   --  E_Class_Wide_Subtype
   --    Direct_Primitive_Operations
   --    Cloned_Subtype                        (subtype case only)
   --    First_Entity
   --    Equivalent_Type                       (always Empty for type)
   --    Non_Limited_View
   --    Last_Entity
   --    SSO_Set_High_By_Default               (base type only)
   --    SSO_Set_Low_By_Default                (base type only)
   --    Corresponding_Remote_Type $$$ type
   --    Renamed_Entity $$$ type
   --    First_Component                       (synth)
   --    First_Component_Or_Discriminant       (synth)
   --    Has_Non_Limited_View                  (synth)
   --    (plus type attributes)

   --  E_Component
   --    Linker_Section_Pragma $$$
   --    Normalized_First_Bit
   --    Current_Value                         (always Empty)
   --    Component_Bit_Offset
   --    Esize
   --    Component_Clause
   --    Normalized_Position
   --    DT_Entry_Count
   --    Entry_Formal
   --    Prival
   --    Renamed_Object                        (always Empty)
   --    Discriminant_Checking_Func
   --    Corresponding_Record_Component
   --    Original_Record_Component
   --    DT_Offset_To_Top_Func
   --    Related_Type
   --    Has_Biased_Representation
   --    Has_Per_Object_Constraint
   --    Is_Atomic
   --    Is_Independent
   --    Is_Return_Object
   --    Is_Tag
   --    Is_Volatile
   --    Is_Volatile_Full_Access
   --    Treat_As_Volatile
   --    Is_Full_Access                      (synth)
   --    Next_Component                      (synth)
   --    Next_Component_Or_Discriminant      (synth)

   --  E_Constant
   --  E_Loop_Parameter
   --    Current_Value                         (always Empty)
   --    Discriminal_Link
   --    Full_View
   --    Esize
   --    Extra_Accessibility                   (constants only)
   --    Alignment
   --    Status_Flag_Or_Transient_Decl
   --    Actual_Subtype
   --    Renamed_Object
   --    Renamed_Entity $$$
   --    Size_Check_Code                       (constants only)
   --    Prival_Link                           (privals only)
   --    Interface_Name                        (constants only)
   --    Related_Type                          (constants only)
   --    Initialization_Statements
   --    BIP_Initialization_Call
   --    Last_Aggregate_Assignment
   --    Activation_Record_Component
   --    Encapsulating_State                   (constants only)
   --    Linker_Section_Pragma
   --    Contract                              (constants only)
   --    SPARK_Pragma                          (constants only)
   --    Has_Alignment_Clause
   --    Has_Atomic_Components
   --    Has_Biased_Representation
   --    Has_Completion                        (constants only)
   --    Has_Independent_Components
   --    Has_Size_Clause
   --    Has_Thunks                            (constants only)
   --    Has_Volatile_Components
   --    Is_Atomic
   --    Is_Elaboration_Checks_OK_Id           (constants only)
   --    Is_Elaboration_Warnings_OK_Id         (constants only)
   --    Is_Eliminated
   --    Is_Finalized_Transient
   --    Is_Ignored_Transient
   --    Is_Independent
   --    Is_Return_Object
   --    Is_True_Constant
   --    Is_Uplevel_Referenced_Entity
   --    Is_Volatile
   --    Is_Volatile_Full_Access
   --    Optimize_Alignment_Space              (constants only)
   --    Optimize_Alignment_Time               (constants only)
   --    SPARK_Pragma_Inherited                (constants only)
   --    Stores_Attribute_Old_Prefix           (constants only)
   --    Treat_As_Volatile
   --    Address_Clause                        (synth)
   --    Alignment_Clause                      (synth)
   --    Is_Elaboration_Target                 (synth)
   --    Is_Full_Access                        (synth)
   --    Size_Clause                           (synth)

   --  E_Decimal_Fixed_Point_Type
   --  E_Decimal_Fixed_Point_Subtype
   --    Scale_Value
   --    Digits_Value
   --    Scalar_Range
   --    Delta_Value
   --    Small_Value
   --    Static_Real_Or_String_Predicate
   --    Has_Machine_Radix_Clause
   --    Machine_Radix_10
   --    Aft_Value                           (synth)
   --    Type_Low_Bound                      (synth)
   --    Type_High_Bound                     (synth)
   --    (plus type attributes)

   --  E_Discriminant
   --    Normalized_First_Bit
   --    Current_Value                         (always Empty)
   --    Component_Bit_Offset
   --    Esize
   --    Component_Clause
   --    Normalized_Position
   --    Discriminant_Number
   --    Discriminal
   --    Renamed_Object                        (always Empty)
   --    Corresponding_Discriminant
   --    Discriminant_Default_Value
   --    Corresponding_Record_Component
   --    Original_Record_Component
   --    CR_Discriminant
   --    Is_Completely_Hidden
   --    Is_Return_Object
   --    Entry_Formal $$$
   --    Linker_Section_Pragma $$$
   --    Next_Component_Or_Discriminant      (synth)
   --    Next_Discriminant                   (synth)
   --    Next_Stored_Discriminant            (synth)

   --  E_Entry
   --  E_Entry_Family
   --    Protected_Body_Subprogram
   --    Barrier_Function
   --    Elaboration_Entity
   --    Entry_Parameters_Type
   --    First_Entity
   --    Alias                                (for entry only. Empty)
   --    Last_Entity
   --    Accept_Address
   --    Scope_Depth_Value
   --    Protection_Object                    (protected kind)
   --    Contract_Wrapper
   --    Extra_Formals
   --    Contract
   --    SPARK_Pragma                         (protected kind)
   --    Default_Expressions_Processed
   --    Entry_Accepted
   --    Has_Yield_Aspect
   --    Has_Expanded_Contract
   --    Ignore_SPARK_Mode_Pragmas
   --    Is_Elaboration_Checks_OK_Id
   --    Is_Elaboration_Warnings_OK_Id
   --    Is_Entry_Wrapper
   --    Needs_No_Actuals
   --    Sec_Stack_Needed_For_Return
   --    SPARK_Pragma_Inherited               (protected kind)
   --    Uses_Sec_Stack
   --    Renamed_Entity $$$
   --    Address_Clause                       (synth)
   --    Entry_Index_Type                     (synth)
   --    First_Formal                         (synth)
   --    First_Formal_With_Extras             (synth)
   --    Is_Elaboration_Target                (synth)
   --    Last_Formal                          (synth)
   --    Number_Formals                       (synth)
   --    Scope_Depth                          (synth)

   --  E_Entry_Index_Parameter
   --    Entry_Index_Constant

   --  E_Enumeration_Literal
   --    Enumeration_Pos
   --    Enumeration_Rep
   --    Alias
   --    Enumeration_Rep_Expr
   --    Interface_Name $$$
   --    Renamed_Object $$$
   --    Esize $$$
   --    Renamed_Entity $$$
   --    Next_Literal                         (synth)

   --  E_Enumeration_Type
   --  E_Enumeration_Subtype
   --    First_Entity $$$ type
   --    Renamed_Object $$$
   --    Lit_Strings                          (root type only)
   --    First_Literal
   --    Lit_Indexes                          (root type only)
   --    Default_Aspect_Value                 (base type only)
   --    Scalar_Range
   --    Lit_Hash                             (root type only)
   --    Enum_Pos_To_Rep                      (type only)
   --    Static_Discrete_Predicate
   --    Has_Biased_Representation
   --    Has_Contiguous_Rep
   --    Has_Enumeration_Rep_Clause
   --    Has_Pragma_Ordered                   (base type only)
   --    Nonzero_Is_True                      (base type only)
   --    No_Predicate_On_Actual
   --    No_Dynamic_Predicate_On_Actual
   --    Type_Low_Bound                       (synth)
   --    Type_High_Bound                      (synth)
   --    (plus type attributes)

   --  E_Exception
   --    Esize
   --    Alignment
   --    Renamed_Entity
   --    Register_Exception_Call
   --    Interface_Name
   --    Activation_Record_Component
   --    Discard_Names
   --    Is_Raised
   --    Renamed_Object $$$

   --  E_Exception_Type
   --    Equivalent_Type
   --    (plus type attributes)

   --  E_Floating_Point_Type
   --  E_Floating_Point_Subtype
   --    Digits_Value
   --    Float_Rep                            (Float_Rep_Kind)
   --    Default_Aspect_Value                 (base type only)
   --    Scalar_Range
   --    Static_Real_Or_String_Predicate
   --    Machine_Emax_Value                   (synth)
   --    Machine_Emin_Value                   (synth)
   --    Machine_Mantissa_Value               (synth)
   --    Machine_Radix_Value                  (synth)
   --    Model_Emin_Value                     (synth)
   --    Model_Epsilon_Value                  (synth)
   --    Model_Mantissa_Value                 (synth)
   --    Model_Small_Value                    (synth)
   --    Safe_Emax_Value                      (synth)
   --    Safe_First_Value                     (synth)
   --    Safe_Last_Value                      (synth)
   --    Type_Low_Bound                       (synth)
   --    Type_High_Bound                      (synth)
   --    (plus type attributes)

   --  E_Function
   --  E_Generic_Function
   --    Mechanism                            (Mechanism_Type)
   --    Handler_Records                      (non-generic case only)
   --    Protected_Body_Subprogram
   --    Next_Inlined_Subprogram
   --    Elaboration_Entity                   (not implicit /=)
   --    DT_Position
   --    DTC_Entity
   --    First_Entity
   --    Alias                                (non-generic case only)
   --    Renamed_Entity
   --    Renamed_Object $$$
   --    Extra_Accessibility_Of_Result        (non-generic case only)
   --    Last_Entity
   --    Interface_Name
   --    Scope_Depth_Value
   --    Generic_Renamings                    (for an instance)
   --    Inner_Instances                      (generic case only)
   --    Inner_Instances $$$ also E_Function
   --    Protection_Object                    (for concurrent kind)
   --    Subps_Index                          (non-generic case only)
   --    Interface_Alias
   --    LSP_Subprogram                       (non-generic case only)
   --    Overridden_Operation
   --    Wrapped_Entity                       (non-generic case only)
   --    Extra_Formals
   --    Anonymous_Masters                    (non-generic case only)
   --    Corresponding_Equality               (implicit /= only)
   --    Thunk_Entity                         (thunk case only)
   --    Corresponding_Procedure              (generate C code only)
   --    Linker_Section_Pragma
   --    Contract
   --    Import_Pragma                        (non-generic case only)
   --    Class_Postconditions
   --    Class_Preconditions
   --    Class_Preconditions_Subprogram
   --    Dynamic_Call_Helper
   --    Ignored_Class_Preconditions
   --    Ignored_Class_Postconditions
   --    Indirect_Call_Wrapper
   --    Static_Call_Helper
   --    Protected_Subprogram                 (non-generic case only)
   --    SPARK_Pragma
   --    Original_Protected_Subprogram
   --    Body_Needed_For_SAL
   --    Contains_Ignored_Ghost_Code
   --    Default_Expressions_Processed
   --    Delay_Cleanups
   --    Delay_Subprogram_Descriptors
   --    Discard_Names
   --    Elaboration_Entity_Required
   --    Has_Completion
   --    Has_Controlling_Result
   --    Has_Expanded_Contract                (non-generic case only)
   --    Has_Master_Entity
   --    Has_Missing_Return
   --    Has_Nested_Block_With_Handler
   --    Has_Nested_Subprogram
   --    Has_Out_Or_In_Out_Parameter
   --    Has_Recursive_Call
   --    Has_Yield_Aspect
   --    Ignore_SPARK_Mode_Pragmas
   --    Is_Abstract_Subprogram               (non-generic case only)
   --    Is_Called                            (non-generic case only)
   --    Is_Class_Wide_Wrapper
   --    Is_Constructor
   --    Is_CUDA_Kernel                       (non-generic case only)
   --    Is_DIC_Procedure                     (non-generic case only)
   --    Is_Discrim_SO_Function
   --    Is_Discriminant_Check_Function
   --    Is_Elaboration_Checks_OK_Id
   --    Is_Elaboration_Warnings_OK_Id
   --    Is_Eliminated
   --    Is_Generic_Actual_Subprogram         (non-generic case only)
   --    Is_Hidden_Non_Overridden_Subpgm      (non-generic case only)
   --    Is_Initial_Condition_Procedure       (non-generic case only)
   --    Is_Inlined_Always                    (non-generic case only)
   --    Is_Instantiated                      (generic case only)
   --    Is_Intrinsic_Subprogram
   --    Is_Invariant_Procedure               (non-generic case only)
   --    Is_Machine_Code_Subprogram           (non-generic case only)
   --    Is_Partial_Invariant_Procedure       (non-generic case only)
   --    Is_Predicate_Function                (non-generic case only)
   --    Is_Primitive
   --    Is_Primitive_Wrapper                 (non-generic case only)
   --    Is_Private_Descendant
   --    Is_Private_Primitive                 (non-generic case only)
   --    Is_Pure
   --    Is_Visible_Lib_Unit
   --    Is_Wrapper
   --    Needs_No_Actuals
   --    Requires_Overriding                  (non-generic case only)
   --    Return_Present
   --    Returns_By_Ref
   --    Rewritten_For_C                      (generate C code only)
   --    Sec_Stack_Needed_For_Return
   --    SPARK_Pragma_Inherited
   --    Uses_Sec_Stack
   --    Address_Clause                       (synth)
   --    First_Formal                         (synth)
   --    First_Formal_With_Extras             (synth)
   --    Is_Elaboration_Target                (synth)
   --    Last_Formal                          (synth)
   --    Number_Formals                       (synth)
   --    Scope_Depth                          (synth)

   --  E_General_Access_Type
   --    First_Entity $$$
   --    Renamed_Entity $$$
   --    Master_Id
   --    Directly_Designated_Type
   --    Associated_Storage_Pool              (root type only)
   --    Finalization_Master                  (root type only)
   --    Storage_Size_Variable                (base type only)
   --    (plus type attributes)

   --  E_Generic_In_Parameter
   --  E_Generic_In_Out_Parameter
   --    Current_Value                        (always Empty)
   --    Entry_Component
   --    Actual_Subtype
   --    Renamed_Object                       (always Empty)
   --    Default_Value
   --    Protected_Formal
   --    Is_Controlling_Formal
   --    Is_Return_Object
   --    Parameter_Mode                       (synth)

   --  E_Incomplete_Type
   --  E_Incomplete_Subtype
   --    Direct_Primitive_Operations
   --    Non_Limited_View
   --    Private_Dependents
   --    Discriminant_Constraint
   --    Stored_Constraint
   --    First_Entity $$$
   --    Last_Entity $$$
   --    Has_Non_Limited_View                 (synth)
   --    (plus type attributes)

   --  E_In_Parameter
   --  E_In_Out_Parameter
   --  E_Out_Parameter
   --    Linker_Section_Pragma $$$
   --    Mechanism                            (Mechanism_Type)
   --    Current_Value
   --    Discriminal_Link                     (discriminals only)
   --    Entry_Component
   --    Esize
   --    Extra_Accessibility
   --    Alignment
   --    Extra_Formal
   --    Unset_Reference
   --    Actual_Subtype
   --    Renamed_Object
   --    Spec_Entity
   --    Default_Value
   --    Default_Expr_Function
   --    Protected_Formal
   --    Extra_Constrained
   --    Minimum_Accessibility
   --    Last_Assignment                      (OUT, IN-OUT only)
   --    Activation_Record_Component
   --    Has_Initial_Value
   --    Is_Controlling_Formal
   --    Is_Only_Out_Parameter
   --    Low_Bound_Tested
   --    Is_Return_Object
   --    Is_Activation_Record
   --    Parameter_Mode                       (synth)

   --  E_Label
   --    Renamed_Object $$$
   --    Renamed_Entity $$$
   --    Enclosing_Scope
   --    Reachable

   --  E_Limited_Private_Type
   --  E_Limited_Private_Subtype
   --    Scalar_Range $$$ type
   --    First_Entity
   --    Private_Dependents
   --    Underlying_Full_View
   --    Last_Entity
   --    Discriminant_Constraint
   --    Stored_Constraint
   --    Has_Completion
   --    (plus type attributes)

   --  E_Loop
   --    First_Exit_Statement
   --    Has_Exit
   --    Has_Loop_Entry_Attributes
   --    Has_Master_Entity
   --    Has_Nested_Block_With_Handler
   --    Uses_Sec_Stack
   --    First_Entity $$$
   --    Last_Entity $$$
   --    Renamed_Object $$$

   --  E_Modular_Integer_Type
   --  E_Modular_Integer_Subtype
   --    Modulus                              (base type only)
   --    Default_Aspect_Value                 (base type only)
   --    Original_Array_Type
   --    Scalar_Range
   --    Static_Discrete_Predicate
   --    Non_Binary_Modulus                   (base type only)
   --    Has_Biased_Representation
   --    Has_Shift_Operator                   (base type only)
   --    No_Predicate_On_Actual
   --    No_Dynamic_Predicate_On_Actual
   --    Type_Low_Bound                       (synth)
   --    Type_High_Bound                      (synth)
   --    (plus type attributes)

   --  E_Named_Integer
   --    Renamed_Object $$$

   --  E_Named_Real

   --  E_Operator
   --    First_Entity
   --    Alias
   --    Extra_Accessibility_Of_Result
   --    Last_Entity
   --    Subps_Index
   --    Overridden_Operation
   --    Linker_Section_Pragma
   --    Contract
   --    Import_Pragma
   --    LSP_Subprogram
   --    SPARK_Pragma
   --    Default_Expressions_Processed
   --    Has_Nested_Subprogram
   --    Ignore_SPARK_Mode_Pragmas
   --    Is_Class_Wide_Wrapper
   --    Is_Elaboration_Checks_OK_Id
   --    Is_Elaboration_Warnings_OK_Id
   --    Is_Intrinsic_Subprogram
   --    Is_Machine_Code_Subprogram
   --    Is_Primitive
   --    Is_Pure
   --    Is_Wrapper
   --    SPARK_Pragma_Inherited
   --    Interface_Name $$$
   --    Renamed_Entity $$$
   --    Renamed_Object $$$
   --    Is_Elaboration_Target                (synth)
   --    Aren't there more flags and fields? seems like this list should be
   --    more similar to the E_Function list, which is much longer ???

   --  E_Ordinary_Fixed_Point_Type
   --  E_Ordinary_Fixed_Point_Subtype
   --    Delta_Value
   --    Default_Aspect_Value                 (base type only)
   --    Scalar_Range
   --    Static_Real_Or_String_Predicate
   --    Small_Value
   --    Has_Small_Clause
   --    Aft_Value                            (synth)
   --    Type_Low_Bound                       (synth)
   --    Type_High_Bound                      (synth)
   --    (plus type attributes)

   --  E_Package
   --  E_Generic_Package
   --    Dependent_Instances                  (for an instance)
   --    Handler_Records                      (non-generic case only)
   --    Generic_Homonym                      (generic case only)
   --    Associated_Formal_Package
   --    Elaboration_Entity
   --    Related_Instance                     (non-generic case only)
   --    First_Private_Entity
   --    First_Entity
   --    Renamed_Entity
   --    Renamed_Object $$$
   --    Body_Entity
   --    Last_Entity
   --    Interface_Name
   --    Scope_Depth_Value
   --    Generic_Renamings                    (for an instance)
   --    Inner_Instances                      (generic case only)
   --    Inner_Instances $$$ also E_Package
   --    Limited_View                         (non-generic/instance)
   --    Incomplete_Actuals                   (for an instance)
   --    Abstract_States
   --    Package_Instantiation
   --    Current_Use_Clause
   --    Finalizer                            (non-generic case only)
   --    Anonymous_Masters                    (non-generic case only)
   --    Contract
   --    SPARK_Pragma
   --    SPARK_Aux_Pragma
   --    Body_Needed_For_Inlining
   --    Body_Needed_For_SAL
   --    Contains_Ignored_Ghost_Code
   --    Delay_Subprogram_Descriptors
   --    Discard_Names
   --    Elaborate_Body_Desirable             (non-generic case only)
   --    Elaboration_Entity_Required
   --    From_Limited_With
   --    Has_All_Calls_Remote
   --    Has_Completion
   --    Has_Forward_Instantiation
   --    Has_Master_Entity
   --    Has_RACW                             (non-generic case only)
   --    Ignore_SPARK_Mode_Pragmas
   --    Is_Called                            (non-generic case only)
   --    Is_Elaboration_Checks_OK_Id
   --    Is_Elaboration_Warnings_OK_Id
   --    Is_Instantiated
   --    In_Package_Body
   --    Is_Private_Descendant
   --    In_Use
   --    Is_Visible_Lib_Unit
   --    Renamed_In_Spec                      (non-generic case only)
   --    SPARK_Aux_Pragma_Inherited
   --    SPARK_Pragma_Inherited
   --    Static_Elaboration_Desired           (non-generic case only)
   --    Renamed_Object $$$
   --    Has_Non_Null_Abstract_State          (synth)
   --    Has_Null_Abstract_State              (synth)
   --    Is_Elaboration_Target                (synth)
   --    Is_Wrapper_Package                   (synth) (non-generic case only)
   --    Has_Limited_View                     (synth) (non-generic case only)
   --    Scope_Depth                          (synth)

   --  E_Package_Body
   --    Handler_Records                      (non-generic case only)
   --    Related_Instance                     (non-generic case only)
   --    First_Entity
   --    Spec_Entity
   --    Last_Entity
   --    Scope_Depth_Value
   --    Finalizer                            (non-generic case only)
   --    Contract
   --    SPARK_Pragma
   --    SPARK_Aux_Pragma
   --    Contains_Ignored_Ghost_Code
   --    Delay_Subprogram_Descriptors
   --    Ignore_SPARK_Mode_Pragmas
   --    SPARK_Aux_Pragma_Inherited
   --    SPARK_Pragma_Inherited
   --    Renamed_Entity $$$
   --    Scope_Depth                          (synth)

   --  E_Private_Type
   --  E_Private_Subtype
   --    Scalar_Range $$$ type
   --    Direct_Primitive_Operations
   --    First_Entity
   --    Private_Dependents
   --    Underlying_Full_View
   --    Last_Entity
   --    Discriminant_Constraint
   --    Stored_Constraint
   --    Has_Completion
   --    Is_Controlled_Active                 (base type only)
   --  $$$above in                            (plus type attributes)
   --    (plus type attributes)

   --  E_Procedure
   --  E_Generic_Procedure
   --    Associated_Node_For_Itype $$$ E_Procedure
   --    Handler_Records                      (non-generic case only)
   --    Protected_Body_Subprogram
   --    Next_Inlined_Subprogram
   --    Elaboration_Entity
   --    DT_Position
   --    DTC_Entity
   --    First_Entity
   --    Alias                                (non-generic case only)
   --    Renamed_Entity
   --    Renamed_Object $$$
   --    Receiving_Entry                      (non-generic case only)
   --    Last_Entity
   --    Interface_Name
   --    Scope_Depth_Value
   --    Generic_Renamings                    (for an instance)
   --    Inner_Instances                      (generic case only)
   --    Inner_Instances $$$ also E_Procedure
   --    Protection_Object                    (for concurrent kind)
   --    Subps_Index                          (non-generic case only)
   --    Interface_Alias
   --    LSP_Subprogram                       (non-generic case only)
   --    Overridden_Operation                 (never for init proc)
   --    Wrapped_Entity                       (non-generic case only)
   --    Extra_Formals
   --    Anonymous_Masters                    (non-generic case only)
   --    Static_Initialization                (init_proc only)
   --    Thunk_Entity                         (thunk case only)
   --    Corresponding_Function               (generate C code only)
   --    Linker_Section_Pragma
   --    Contract
   --    Import_Pragma                        (non-generic case only)
   --    Class_Postconditions
   --    Class_Preconditions
   --    Class_Preconditions_Subprogram
   --    Dynamic_Call_Helper
   --    Ignored_Class_Preconditions
   --    Ignored_Class_Postconditions
   --    Indirect_Call_Wrapper
   --    Static_Call_Helper
   --    Protected_Subprogram                 (non-generic case only)
   --    SPARK_Pragma
   --    Original_Protected_Subprogram
   --    Body_Needed_For_SAL
   --    Contains_Ignored_Ghost_Code
   --    Delay_Cleanups                      $$$Dup below
   --    Discard_Names                       $$$Dup below
   --    Elaboration_Entity_Required
   --    Default_Expressions_Processed
   --    Delay_Cleanups
   --    Delay_Subprogram_Descriptors
   --    Discard_Names
   --    Has_Completion
   --    Has_Expanded_Contract                (non-generic case only)
   --    Has_Master_Entity
   --    Has_Nested_Block_With_Handler
   --    Has_Nested_Subprogram
   --    Has_Yield_Aspect
   --    Ignore_SPARK_Mode_Pragmas
   --    Is_Abstract_Subprogram               (non-generic case only)
   --    Is_Asynchronous
   --    Is_Called                            (non-generic case only)
   --    Is_Class_Wide_Wrapper
   --    Is_Constructor
   --    Is_CUDA_Kernel
   --    Is_DIC_Procedure                     (non-generic case only)
   --    Is_Elaboration_Checks_OK_Id
   --    Is_Elaboration_Warnings_OK_Id
   --    Is_Eliminated
   --    Is_Generic_Actual_Subprogram         (non-generic case only)
   --    Is_Hidden_Non_Overridden_Subpgm      (non-generic case only)
   --    Is_Initial_Condition_Procedure       (non-generic case only)
   --    Is_Inlined_Always                    (non-generic case only)
   --    Is_Instantiated                      (generic case only)
   --    Is_Interrupt_Handler
   --    Is_Intrinsic_Subprogram
   --    Is_Invariant_Procedure               (non-generic case only)
   --    Is_Machine_Code_Subprogram           (non-generic case only)
   --    Is_Null_Init_Proc
   --    Is_Partial_DIC_Procedure             (synth) (non-generic case only)
   --    Is_Partial_Invariant_Procedure       (non-generic case only)
   --    Is_Predicate_Function                (non-generic case only)
   --    Is_Primitive
   --    Is_Primitive_Wrapper                 (non-generic case only)
   --    Is_Private_Descendant
   --    Is_Private_Primitive                 (non-generic case only)
   --    Is_Pure
   --    Is_Wrapper
   --    Is_Valued_Procedure
   --    Is_Visible_Lib_Unit
   --    Needs_No_Actuals
   --    No_Return
   --    Requires_Overriding                  (non-generic case only)
   --    Sec_Stack_Needed_For_Return
   --    SPARK_Pragma_Inherited
   --    Entry_Parameters_Type $$$
   --    Address_Clause                       (synth)
   --    First_Formal                         (synth)
   --    First_Formal_With_Extras             (synth)
   --    Is_Elaboration_Target                (synth)
   --    Is_Finalizer                         (synth)
   --    Last_Formal                          (synth)
   --    Number_Formals                       (synth)

   --  E_Protected_Body
   --    SPARK_Pragma
   --    Ignore_SPARK_Mode_Pragmas
   --    SPARK_Pragma_Inherited
   --    (any others??? First/Last Entity, Scope_Depth???)

   --  E_Protected_Object$$$No such thing

   --  E_Protected_Type
   --  E_Protected_Subtype
   --    Direct_Primitive_Operations
   --    First_Private_Entity
   --    First_Entity
   --    Corresponding_Record_Type
   --    Entry_Bodies_Array
   --    Last_Entity
   --    Discriminant_Constraint
   --    Scope_Depth_Value
   --    Stored_Constraint
   --    Anonymous_Object
   --    Contract
   --    Entry_Max_Queue_Lengths_Array
   --    SPARK_Aux_Pragma
   --    Ignore_SPARK_Mode_Pragmas
   --    SPARK_Aux_Pragma_Inherited
   --    Uses_Lock_Free
   --    First_Component                      (synth)
   --    First_Component_Or_Discriminant      (synth)
   --    Has_Entries                          (synth)
   --    Has_Interrupt_Handler                (synth)
   --    Number_Entries                       (synth)
   --    Scope_Depth                          (synth)
   --    (plus type attributes)

   --  E_Record_Type
   --  E_Record_Subtype
   --    Renamed_Entity $$$ type
   --    Interface_Name $$$ type
   --    Direct_Primitive_Operations
   --    Access_Disp_Table                    (base type only)
   --    Cloned_Subtype                       (subtype case only)
   --    First_Entity
   --    Corresponding_Concurrent_Type
   --    Parent_Subtype                       (base type only)
   --    Last_Entity
   --    Discriminant_Constraint
   --    Corresponding_Remote_Type
   --    Stored_Constraint
   --    Interfaces
   --    Dispatch_Table_Wrappers              (base type only)
   --    Underlying_Record_View               (base type only)
   --    Access_Disp_Table_Elab_Flag          (base type only)
   --    Predicated_Parent                    (subtype only)
   --    Component_Alignment                  (special)  (base type only)
   --    C_Pass_By_Copy                       (base type only)
   --    Has_Dispatch_Table                   (base tagged type only)
   --    Has_Pragma_Pack                      (impl base type only)
   --    Has_Private_Ancestor
   --    Has_Private_Extension
   --    Has_Record_Rep_Clause                (base type only)
   --    Has_Static_Discriminants             (subtype only)
   --    Is_Class_Wide_Equivalent_Type
   --    Is_Concurrent_Record_Type
   --    Is_Constrained
   --    Is_Controlled_Active                 (base type only)
   --  $$$above in                            (plus type attributes)
   --    Is_Interface
   --    Is_Limited_Interface
   --    No_Reordering                        (base type only)
   --    Reverse_Bit_Order                    (base type only)
   --    Reverse_Storage_Order                (base type only)
   --    SSO_Set_High_By_Default              (base type only)
   --    SSO_Set_Low_By_Default               (base type only)
   --    First_Component                      (synth)
   --    First_Component_Or_Discriminant      (synth)
   --    (plus type attributes)

   --  E_Record_Type_With_Private
   --  E_Record_Subtype_With_Private
   --    Corresponding_Remote_Type $$$ E_Record_Subtype_With_Private
   --    Direct_Primitive_Operations
   --    First_Entity
   --    Private_Dependents
   --    Underlying_Full_View
   --    Last_Entity
   --    Discriminant_Constraint
   --    Stored_Constraint
   --    Interfaces
   --    Underlying_Record_View $$$           (base type only)
   --    Predicated_Parent                    (subtype only)
   --    Has_Completion
   --    Has_Private_Ancestor
   --    Has_Private_Extension
   --    Has_Record_Rep_Clause                (base type only)
   --    Is_Concurrent_Record_Type
   --    Is_Constrained
   --    Is_Controlled_Active                 (base type only)
   --  $$$above in                            (plus type attributes)
   --    Is_Interface
   --    Is_Limited_Interface
   --    No_Reordering                        (base type only)
   --    Reverse_Bit_Order                    (base type only)
   --    Reverse_Storage_Order                (base type only)
   --    SSO_Set_High_By_Default              (base type only)
   --    SSO_Set_Low_By_Default               (base type only)
   --    Corresponding_Remote_Type $$$ type
   --    First_Component                      (synth)
   --    First_Component_Or_Discriminant      (synth)
   --    (plus type attributes)

   --  E_Return_Statement
   --    Return_Applies_To
   --    First_Entity $$$
   --    Last_Entity $$$

   --  E_Signed_Integer_Type
   --  E_Signed_Integer_Subtype
   --    Renamed_Object $$$ subtype
   --    Interface_Name $$$ subtype
   --    Direct_Primitive_Operations $$$ type
   --    First_Entity $$$
   --    Default_Aspect_Value                 (base type only)
   --    Scalar_Range
   --    Static_Discrete_Predicate
   --    Has_Biased_Representation
   --    Has_Shift_Operator                   (base type only)
   --    No_Predicate_On_Actual
   --    No_Dynamic_Predicate_On_Actual
   --    Type_Low_Bound                       (synth)
   --    Type_High_Bound                      (synth)
   --    (plus type attributes)

   --  E_String_Literal_Subtype
   --    String_Literal_Length
   --    First_Index                          (always Empty)
   --    String_Literal_Low_Bound
   --    Packed_Array_Impl_Type
   --    (plus type attributes)

   --  E_Subprogram_Body
   --    Mechanism
   --    First_Entity
   --    Corresponding_Protected_Entry
   --    Last_Entity
   --    Scope_Depth_Value
   --    Extra_Formals
   --    Anonymous_Masters
   --    Contract
   --    SPARK_Pragma
   --    Contains_Ignored_Ghost_Code
   --    SPARK_Pragma_Inherited
   --    Interface_Name $$$
   --    Renamed_Entity $$$
   --    Scope_Depth                          (synth)

   --  E_Subprogram_Type
   --    Extra_Accessibility_Of_Result
   --    Directly_Designated_Type
   --    Extra_Formals
   --    Access_Subprogram_Wrapper
   --    First_Formal                         (synth)
   --    First_Formal_With_Extras             (synth)
   --    Last_Formal                          (synth)
   --    Number_Formals                       (synth)
   --    Returns_By_Ref
   --    First_Entity $$$
   --    Last_Entity $$$
   --    Interface_Name $$$
   --    (plus type attributes)

   --  E_Task_Body
   --    Contract
   --    SPARK_Pragma
   --    Ignore_SPARK_Mode_Pragmas
   --    SPARK_Pragma_Inherited
   --    First_Entity $$$
   --    (any others??? First/Last Entity, Scope_Depth???)

   --  E_Task_Type
   --  E_Task_Subtype
   --    Direct_Primitive_Operations
   --    First_Private_Entity
   --    First_Entity
   --    Corresponding_Record_Type
   --    Last_Entity
   --    Discriminant_Constraint
   --    Scope_Depth_Value
   --    Stored_Constraint
   --    Task_Body_Procedure
   --    Storage_Size_Variable                (base type only)
   --    Relative_Deadline_Variable           (base type only)
   --    Anonymous_Object
   --    Contract
   --    SPARK_Aux_Pragma
   --    Delay_Cleanups
   --    Has_Master_Entity
   --    Has_Storage_Size_Clause              (base type only)
   --    Ignore_SPARK_Mode_Pragmas
   --    Is_Elaboration_Checks_OK_Id
   --    Is_Elaboration_Warnings_OK_Id
   --    SPARK_Aux_Pragma_Inherited
   --    First_Component                      (synth)
   --    First_Component_Or_Discriminant      (synth)
   --    Has_Entries                          (synth)
   --    Is_Elaboration_Target                (synth)
   --    Number_Entries                       (synth)
   --    Scope_Depth                          (synth)
   --    (plus type attributes)

   --  E_Variable
   --    Hiding_Loop_Variable
   --    Current_Value
   --    Part_Of_Constituents
   --    Part_Of_References
   --    Esize
   --    Extra_Accessibility
   --    Alignment
   --    Status_Flag_Or_Transient_Decl        (transient object only)
   --    Unset_Reference
   --    Actual_Subtype
   --    Renamed_Object
   --    Renamed_Entity $$$
   --    Discriminal_Link $$$
   --    Size_Check_Code
   --    Prival_Link
   --    Interface_Name
   --    Shared_Var_Procs_Instance
   --    Extra_Constrained
   --    Related_Expression
   --    Debug_Renaming_Link
   --    Last_Assignment
   --    Related_Type
   --    Initialization_Statements
   --    BIP_Initialization_Call
   --    Last_Aggregate_Assignment
   --    Activation_Record_Component
   --    Encapsulating_State
   --    Linker_Section_Pragma
   --    Contract
   --    Anonymous_Designated_Type
   --    Validated_Object
   --    SPARK_Pragma
   --    Has_Alignment_Clause
   --    Has_Atomic_Components
   --    Has_Biased_Representation
   --    Has_Independent_Components
   --    Has_Initial_Value
   --    Has_Size_Clause
   --    Has_Volatile_Components
   --    Is_Atomic
   --    Is_Elaboration_Checks_OK_Id
   --    Is_Elaboration_Warnings_OK_Id
   --    Is_Eliminated
   --    Is_Finalized_Transient
   --    Is_Ignored_Transient
   --    Is_Independent
   --    Is_Return_Object
   --    Is_Safe_To_Reevaluate
   --    Is_Shared_Passive
   --    Is_True_Constant
   --    Is_Uplevel_Referenced_Entity
   --    Is_Volatile
   --    Is_Volatile_Full_Access
   --    OK_To_Rename
   --    Optimize_Alignment_Space
   --    Optimize_Alignment_Time
   --    SPARK_Pragma_Inherited
   --    Suppress_Initialization
   --    Treat_As_Volatile
   --    Address_Clause                       (synth)
   --    Alignment_Clause                     (synth)
   --    Is_Elaboration_Target                (synth)
   --    Is_Full_Access                       (synth)
   --    Size_Clause                          (synth)

   --  E_Void
   --    Since E_Void is the initial Ekind value of an entity when it is first
   --    created, one might expect that no attributes would be defined on such
   --    an entity until its Ekind field is set. However, in practice, there
   --    are many instances in which fields of an E_Void entity are set in the
   --    code prior to setting the Ekind field. This is not well documented or
   --    well controlled, and needs cleaning up later. Meanwhile, the access
   --    procedures in the body of Einfo permit many, but not all, attributes
   --    to be applied to an E_Void entity, precisely so that this kind of
   --    pre-setting of attributes works. This is really a hole in the dynamic
   --    type checking, since there is no assurance that the eventual Ekind
   --    value will be appropriate for the attributes set, and the consequence
   --    is that the dynamic type checking in the Einfo body is unnecessarily
   --    weak.
   --
   --    The following are examples of getters and setters called with E_Void:
   --    Entry_Formal $$$
   --    Esize $$$
   --    First_Entity $$$
   --    Handler_Records $$$
   --    Interface_Name $$$
   --    Last_Entity $$$
   --    Renamed_Entity $$$
   --    Renamed_Object $$$
   --    Scalar_Range $$$
   --    Set_Associated_Node_For_Itype $$$
   --    Set_Debug_Renaming_Link $$$
   --    Set_Entry_Cancel_Parameter $$$
   --    Set_First_Entity $$$
   --    Set_Inner_Instances $$$
   --    Set_Last_Entity $$$
   --    Set_Scalar_Range $$$
   --    Set_Entry_Cancel_Parameter $$$

   ---------------
   -- Iterators --
   ---------------

   --  In addition to attributes that are stored as plain data, other
   --  attributes are procedural, and require some small amount of
   --  computation. Of course, from the point of view of a user of this
   --  package, the distinction is not visible (even the field information
   --  provided below should be disregarded, as it is subject to change
   --  without notice). A number of attributes appear as lists: lists of
   --  formals, lists of actuals, of discriminants, etc. For these, pairs
   --  of functions are defined, which take the form:

   --      function First_Thing (E : Enclosing_Construct) return Thing;
   --      function Next_Thing (T : Thing) return Thing;

   --  The end of iteration is always signaled by a value of Empty, so that
   --  loops over these chains invariably have the form:

   --      This : Thing;
   --      ...
   --      This := First_Thing (E);

   --      while Present (This) loop
   --         Do_Something_With (This);
   --        ...
   --        This := Next_Thing (This);
   --      end loop;

   -----------------------------------
   -- Handling of Check Suppression --
   -----------------------------------

   --  There are three ways that checks can be suppressed:

   --    1.  At the command line level
   --    2.  At the scope level.
   --    3.  At the entity level.

   --  See spec of Sem in sem.ads for details of the data structures used
   --  to keep track of these various methods for suppressing checks.

   -------------------------------
   -- Handling of Discriminants --
   -------------------------------

   --  During semantic processing, discriminants are separate entities which
   --  reflect the semantic properties and allowed usage of discriminants in
   --  the language.

   --  In the case of discriminants used as bounds, the references are handled
   --  directly, since special processing is needed in any case. However, there
   --  are two circumstances in which discriminants are referenced in a quite
   --  general manner, like any other variables:

   --     In initialization expressions for records. Note that the expressions
   --     used in Priority, Storage_Size, Task_Info and Relative_Deadline
   --     pragmas are effectively in this category, since these pragmas are
   --     converted to initialized record fields in the Corresponding_Record_
   --     Type.

   --     In task and protected bodies, where the discriminant values may be
   --     referenced freely within these bodies. Discriminants can also appear
   --     in bounds of entry families and in defaults of operations.

   --  In both these cases, the discriminants must be treated essentially as
   --  objects. The following approach is used to simplify and minimize the
   --  special processing that is required.

   --  When a record type with discriminants is analyzed, semantic processing
   --  creates the entities for the discriminants. It also creates additional
   --  sets of entities called discriminals, one for each of the discriminants,
   --  and the Discriminal field of the discriminant entity points to this
   --  additional entity, which is initially created as an uninitialized
   --  (E_Void) entity.

   --  During expansion of expressions, any discriminant reference is replaced
   --  by a reference to the corresponding discriminal. When the initialization
   --  procedure for the record is created (there will always be one, since
   --  discriminants are present, see Exp_Ch3 for further details), the
   --  discriminals are used as the entities for the formal parameters of
   --  this initialization procedure. The references to these discriminants
   --  have already been replaced by references to these discriminals, which
   --  are now the formal parameters corresponding to the required objects.

   --  In the case of a task or protected body, the semantics similarly creates
   --  a set of discriminals for the discriminants of the task or protected
   --  type. When the procedure is created for the task body, the parameter
   --  passed in is a reference to the task value type, which contains the
   --  required discriminant values. The expander creates a set of declarations
   --  of the form:

   --      discr_nameD : constant discr_type renames _task.discr_name;

   --  where discr_nameD is the discriminal entity referenced by the task
   --  discriminant, and _task is the task value passed in as the parameter.
   --  Again, any references to discriminants in the task body have been
   --  replaced by the discriminal reference, which is now an object that
   --  contains the required value.

   --  This approach for tasks means that two sets of discriminals are needed
   --  for a task type, one for the initialization procedure, and one for the
   --  task body. This works out nicely, since the semantics allocates one set
   --  for the task itself, and one set for the corresponding record.

   --  The one bit of trickiness arises in making sure that the right set of
   --  discriminals is used at the right time. First the task definition is
   --  processed. Any references to discriminants here are replaced by the
   --  corresponding *task* discriminals (the record type doesn't even exist
   --  yet, since it is constructed as part of the expansion of the task
   --  declaration, which happens after the semantic processing of the task
   --  definition). The discriminants to be used for the corresponding record
   --  are created at the same time as the other discriminals, and held in the
   --  CR_Discriminant field of the discriminant. A use of the discriminant in
   --  a bound for an entry family is replaced with the CR_Discriminant because
   --  it controls the bound of the entry queue array which is a component of
   --  the corresponding record.

   --  Just before the record initialization routine is constructed, the
   --  expander exchanges the task and record discriminals. This has two
   --  effects. First the generation of the record initialization routine
   --  uses the discriminals that are now on the record, which is the set
   --  that used to be on the task, which is what we want.

   --  Second, a new set of (so far unused) discriminals is now on the task
   --  discriminants, and it is this set that will be used for expanding the
   --  task body, and also for the discriminal declarations at the start of
   --  the task body.

   ---------------------------------------------------
   -- Handling of private data in protected objects --
   ---------------------------------------------------

   --  Private components in protected types pose problems similar to those
   --  of discriminants. Private data is visible and can be directly referenced
   --  from protected bodies. However, when protected entries and subprograms
   --  are expanded into corresponding bodies and barrier functions, private
   --  components lose their original context and visibility.

   --  To remedy this side effect of expansion, private components are expanded
   --  into renamings called "privals", by analogy with "discriminals".

   --     private_comp : comp_type renames _object.private_comp;

   --  Prival declarations are inserted during the analysis of subprogram and
   --  entry bodies to ensure proper visibility for any subsequent expansion.
   --  _Object is the formal parameter of the generated corresponding body or
   --  a local renaming which denotes the protected object obtained from entry
   --  parameter _O. Privals receive minimal decoration upon creation and are
   --  categorized as either E_Variable for the general case or E_Constant when
   --  they appear in functions.

   --  Along with the local declarations, each private component carries a
   --  placeholder which references the prival entity in the current body. This
   --  form of indirection is used to resolve name clashes of privals and other
   --  locally visible entities such as parameters, local objects, entry family
   --  indexes or identifiers used in the barrier condition.

   --  When analyzing the statements of a protected subprogram or entry, any
   --  reference to a private component must resolve to the locally declared
   --  prival through normal visibility. In case of name conflicts (the cases
   --  above), the prival is marked as hidden and acts as a weakly declared
   --  entity. As a result, the reference points to the correct entity. When a
   --  private component is denoted by an expanded name (prot_type.comp for
   --  example), the expansion mechanism uses the placeholder of the component
   --  to correct the Entity and Etype of the reference.

end Einfo;
