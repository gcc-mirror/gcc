.. _Implementation_of_Ada_2022_Features:

***********************************
Implementation of Ada 2022 Features
***********************************

.. index:: Ada 2022 implementation status

.. index:: -gnat22 option (gcc)

.. index:: pragma Ada_2022

.. index:: configuration pragma Ada_2022

.. index:: Ada_2022 configuration pragma

This chapter contains a complete list of Ada 2022 features that have been
implemented. Generally, these features are only available if the *-gnat22* (Ada 2022 features enabled) option is set, or if the configuration pragma ``Ada_2022`` is used.

However, new pragmas, attributes, and restrictions are unconditionally available, since the Ada standard allows the addition of new pragmas, attributes, and restrictions (there are exceptions, which are
documented in the individual descriptions), and also certain packages
were made available in earlier versions of Ada.

An ISO date (YYYY-MM-DD) appears in parentheses on the description line.
This date shows the implementation date of the feature. Any wavefront
subsequent to this date will contain the indicated feature, as will any
subsequent releases. A date of 0000-00-00 means that GNAT has always
implemented the feature, or implemented it as soon as it appeared as a
binding interpretation.

Each feature corresponds to an Ada Issue ('AI') approved by the Ada
standardization group (ISO/IEC JTC1/SC22/WG9) for inclusion in Ada 2022.

The section "RM references" lists all modified paragraphs in the Ada 2012 reference manual. The details of each modification as well as a complete description of the AIs may be found in
http://www.ada-auth.org/AI12-SUMMARY.HTML.

.. index:: AI12-0001 (Ada 2022 feature)

* *AI12-0001 Independence and Representation clauses for atomic objects (2019-11-27)*

  The compiler accepts packing clauses in all cases, even if they have effectively no influence on the layout. Types, where packing is essentially infeasible are, for instance atomic, aliased and by-reference types.

  RM references: 13.02 (6.1/2) 13.02 (7) 13.02 (8) 13.02 (9/3) C.06 (8.1/3)
  C.06 (10) C.06 (11) C.06 (21) C.06 (24)

.. index:: AI12-0003 (Ada 2022 feature)

* *AI12-0003 Specifying the standard storage pool (2020-06-25)*

  Allows the standard storage pool being specified with a ``Default_Storage_Pool`` pragma or aspect.

  RM references: 8.02 (11) 13.11.03 (1/3) 13.11.03 (3.1/3) 13.11.03 (4/3)
  13.11.03 (4.1/3) 13.11.03 (5/3) 13.11.03 (6.2/3) 13.11.03
  (6.3/3)

.. index:: AI12-0004 (Ada 2022 feature)

* *AI12-0004 Normalization and allowed characters for identifiers (2020-06-11)*

  This AI clarifies that Ada identifiers containing characters which are not
  allowed in Normalization Form KC are illegal.

  RM references: 2.01 (4.1/3) 2.03 (4/3) A.03.02 (4/3) A.03.02 (32.5/3)
  A.03.05 (18/3) A.03.05 (51/3)

.. index:: AI12-0020 (Ada 2022 feature)

* *AI12-0020 'Image for all types (2020-03-30)*

  Put_Image prints out a human-readable representation of an object. The
  functionality in Ada2022 RM is fully implemented except the support for
  types in the ``Remote_Types`` packages.

  RM references: 4.10 (0) 3.05 (27.1/2) 3.05 (27.2/2) 3.05 (27.3/2) 3.05
  (27.4/2) 3.05 (27.5/2) 3.05 (27.6/2) 3.05 (27.7/2) 3.05 (28) 3.05
  (29) 3.05 (30/3) 3.05 (31) 3.05 (32) 3.05 (33/3) 3.05 (37.1/2)
  3.05 (38) 3.05 (39) 3.05 (43/3) 3.05 (55/3) 3.05 (55.1/5) 3.05
  (55.2/4) 3.05 (55.3/4) 3.05 (55.4/4) 3.05 (59) H.04 (23) H.04 (23.8/2)

.. index:: AI12-0022 (Ada 2022 feature)

* *AI12-0022 Raise_Expressions (2013-01-27)*

  This feature allows you to write "raise NAME [with STRING]" in an
  expression to rise given exception. It is particularly useful in the case of
  assertions such as preconditions allowing to specify which exception a
  precondition raises if it fails.

  RM references: 4.04 (3/3) 11.02 (6) 11.03 (2/2) 11.03 (3) 11.03 (3.1/2)
  11.03 (4/2) 11.04.01 (10.1/3)

.. index:: AI12-0027 (Ada 2022 feature)

* *AI12-0027 Access values should never designate unaliased components (2020-06-15)*

  AI12-0027 adds a requirement for a value conversion that converts from an array of unaliased components to an array of aliased components to make a copy. It defines such conversions to have a local accessibility, effectively preventing the possibility of unsafe accesses to unaliased components.

  RM references: 4.06 (24.17/3) 4.06 (24.21/2) 4.06 (58) 6.02 (10/3) 3.10.02 (10/3)

.. index:: AI12-0028 (Ada 2022 feature)

* *AI12-0028 Import of variadic C functions (2020-03-03)*

  Ada programs can now properly call variadic C functions by means of the
  conventions C_Variadic_<n>, for small integer values <n>.

  RM references: B.03 (1/3) B.03 (60.15/3) B.03 (75)

.. index:: AI12-0030 (Ada 2022 feature)

* *AI12-0030 Formal derived types and stream attribute availability (2020-08-21)*

  Corner cases involving streaming operations for formal derived limited types
  that are now defined to raise Program_Error. Before, behavior in these cases
  was undefined. Stream attribute availability is more precisely computed in cases where a derived type declaration occurs ahead of a streaming attribute specification for the parent type.

  RM references: 12.05.01 (21/3) 13.13.02 (49/2)

.. index:: AI12-0031 (Ada 2022 feature)

* *AI12-0031 All_Calls_Remote and indirect calls (0000-00-00)*

  Remote indirect calls (i.e., calls through a remote access-to-subprogram type)
  behave the same as remote direct calls.

  RM references: E.02.03 (19/3)

.. index:: AI12-0032 (Ada 2022 feature)

* *AI12-0032 Questions on 'Old (2020-04-24)*

  AI12-0032 resolves several issues related to the 'Old attribute. The GNAT
  compiler already implemented what the AI requires in most of those cases, but two having to do with static and dynamic checking of the accessibility level of the constant object implicitly declared for an 'Old attribute reference were not yet implemented. Accessibility checking for these constants is now implemented as defined in the AI.

  RM references: 4.01.03 (9/3) 6.01.01 (22/3) 6.01.01 (26/3) 6.01.01 (35/3)

.. index:: AI12-0033 (Ada 2022 feature)

* *AI12-0033 Sets of CPUs when defining dispatching domains (0000-00-00)*

  The set of CPUs associated with a dispatching domain is no longer required
  to be a contiguous range of CPU values.

  RM references: D.16.01 (7/3) D.16.01 (9/3) D.16.01 (20/3) D.16.01 (23/3)
  D.16.01 (24/3) D.16.01 (26/3)

.. index:: AI12-0035 (Ada 2022 feature)

* *AI12-0035 Accessibility checks for indefinite elements of containers (0000-00-00)*

  If the element type for an instance of one of the indefinite container generics has an access discriminant, then accessibility checks (at run-time) prevent inserting a value into a container object if the value's discriminant designates an object that is too short-lived (that is, if the designated object has an accessibility level that is deeper than that of the instance). Without this check, dangling references would be possible. GNAT handled this correctly already before this AI was issued.

  RM references: A.18 (5/3) A.18.11 (8/2) A.18.12 (7/2) A.18.13 (8/2)
  A.18.14 (8/2) A.18.15 (4/2) A.18.16 (4/2) A.18.17 (7/3) A.18.18
  (39/3) A.18.18 (47/3)

.. index:: AI12-0036 (Ada 2022 feature)

* *AI12-0036 The actual for an untagged formal derived type cannot be tagged (2019-10-21)*

  AI12-0036 is a binding interpretation that adds the following legality rule:
  The actual type for a formal derived type shall be tagged if and only if the
  formal derived type is a private extension. The check is implemented for all Ada dialects, not just Ada 2022.

  RM references: 12.05.01 (5.1/3)

.. index:: AI12-0037 (Ada 2022 feature)

* *AI12-0037 New types in Ada.Locales can't be converted to/from strings (2016-09-10)*

  The type definitions for Language_Code and Country_Code are now using dynamic
  predicates.

  RM references: A.19 (4/3)

.. index:: AI12-0039 (Ada 2022 feature)

* *AI12-0039 Ambiguity in syntax for membership expression removed (0000-00-00)*

  An ambiguity in the syntax for membership expressions was resolved. For example, "A in B and C" can be parsed in only one way because of this AI.

  RM references: 4.04 (3/3) 4.04 (3.2/3) 4.05.02 (3.1/3) 4.05.02 (4) 4.05.02
  (4.1/3) 4.05.02 (27/3) 4.05.02 (27.1/3) 4.05.02 (28.1/3) 4.05.02
  (28.2/3) 4.05.02 (29/3) 4.05.02 (30/3) 4.05.02 (30.1/3) 4.05.02
  (30.2/3) 4.05.02 (30.3/3) 4.09 (11/3) 4.09 (32.6/3) 8.06 (27.1/3)
  3.02.04 (17/3)

.. index:: AI12-0040 (Ada 2022 feature)

* *AI12-0040 Resolving the selecting_expression of a case_expression (0000-00-00)*

  The definition of "complete context" is corrected so that selectors of case expressions
  and of case statements are treated uniformly.

  RM references: 8.06 (9)

.. index:: AI12-0041 (Ada 2022 feature)

* *AI12-0041 Type_Invariant'Class for interface types (2016-12-12)*

  Subprogram calls within class-wide type invariant expressions get resolved
  as primitive operations instead of being dynamically dispatched.

  RM references: 7.03.02 (1/3) 7.03.02 (3/3)

.. index:: AI12-0042 (Ada 2022 feature)

* *AI12-0042 Type invariant checking rules (2020-06-05)*

  AI12-0042 adds rules for type invariants.
  Specifically, when inheriting a private dispatching operation when the ancestor operation is visible at the point of the type extension, the operation must be abstract or else overridden. In addition, for a class-wide view conversion from an object of a specific type T to which a type invariant applies, an invariant check is performed when the conversion is within the immediate scope of T.

  RM references: 7.03.02 (6/3) 7.03.02 (17/3) 7.03.02 (18/3) 7.03.02 (19/3)
  7.03.02 (20/3)

.. index:: AI12-0043 (Ada 2022 feature)

* *AI12-0043 Details of the storage pool used when Storage_Size is specified (0000-00-00)*

  Clarify that a Storage_Size specification for an access type specifies both an upper bound and a lower bound (not just a lower bound) of the amount of storage allowed for allocated objects.

  RM references: 13.11 (18)

.. index:: AI12-0044 (Ada 2022 feature)

* *AI12-0044 Calling visible functions from type invariant expressions (2020-05-11)*

  AI05-0289-1 extends invariant checking to `in` parameters. However, this makes
  it impossible to call a public function of the type from an invariant
  expression, as that public function will attempt to check the invariant,
  resulting in an infinite recursion.

  This AI specifies, that type-invariant checking is performed on parameters
  of mode `in` upon return from procedure calls, but not of `in`-mode
  parameters in functions.

  RM references: 7.03.02 (19/3)

.. index:: AI12-0045 (Ada 2022 feature)

* *AI12-0045 Pre- and Postconditions are allowed on generic subprograms (2015-03-17)*

  The SPARK toolset now supports contracts on generic subprograms, packages and
  their respective bodies.

  RM references: 6.01.01 (1/3)

.. index:: AI12-0046 (Ada 2022 feature)

* *AI12-0046 Enforcing legality for anonymous access components in record aggregates (0000-00-00)*

  For a record aggregate of the form (X | Y => ....), any relevant legality rules are checked for both for X and Y.

  For example,

  .. code::

      X : aliased constant String := ... ;
      type R is record
        F1 : access constant String;
        F2 : access String;
      end record;
      Obj : R := (F1 | F2 => X'Access); -- ok for F1, but illegal for F2

  RM references: 4.03.01 (16/3)

.. index:: AI12-0047 (Ada 2022 feature)

* *AI12-0047 Generalized iterators and discriminant-dependent components (0000-00-00)*

  Iterating over the elements of an array is subject to the same legality checks as renaming the array. For example, if an assignment to an enclosing discriminated object could cause an array object to cease to exist then we don't allow renaming the array. So it is similarly not allowed to iterate over the elements of such an array.

  RM references: 5.05.02 (6/3)

.. index:: AI12-0048 (Ada 2022 feature)

* *AI12-0048 Default behavior of tasks on a multiprocessor with a specified dispatching policy (0000-00-00)*

  Clarify that if the user does not impose requirements about what CPUs a given task might execute on, then the implementation does not get to impose such requirements. This avoids potential problems with priority inversion.

  RM references: D.16.01 (30/3)

.. index:: AI12-0049 (Ada 2022 feature)

* *AI12-0049 Invariants need to be checked on the initialization of deferred constants (0000-00-00)*

  Invariant checking for deferred constants (and subcomponents thereof) is performed. Corrects a clear oversight in the previous RM wording.

  RM references: 7.03.02 (10/3)

.. index:: AI12-0050 (Ada 2022 feature)

* *AI12-0050 Conformance of quantified expressions (2016-07-22)*

  Compiler rejects a subprogram body when an expression for a boolean formal
  parameter includes a quantified expression, and the subprogram declaration
  contains a textual copy of the same.

  RM references: 6.03.01 (20) 6.03.01 (21)

.. index:: AI12-0051 (Ada 2022 feature)

* *AI12-0051 The Priority aspect can be specified when Attach_Handler is specified (0000-00-00)*

  Previous RM wording had two contradictory rules for determining (in some cases) the priority of a protected subprogram that is attached to an interrupt. The AI clarifies which one of the rules takes precedence.

  RM references: D.03 (10/3)

.. index:: AI12-0052 (Ada 2022 feature)

* *AI12-0052 Implicit objects are considered overlapping (0000-00-00)*

  Clarify that the rules about unsynchronized concurrent access apply as one would expect in the case of predefined routines that access Text_IO's default input and default output files. There was no compiler changes needed to implement this.

  RM references: A (3/2) A.10.03 (21)

.. index:: AI12-0054-2 (Ada 2022 feature)

* *AI12-0054-2 Aspect Predicate_Failure (0000-00-00)*

  New aspect Predicate_Failure is defined.  A solution for the problem that a predicate like

  ..  code::

    subtype Open_File is File with Dynamic_Predicate =\> Is_Open (Open_File) or else (raise File_Not_Open);

  does the wrong thing in the case of a membership test.

  RM references: 3.02.04 (14/3) 3.02.04 (31/3) 3.02.04 (35/3)

.. index:: AI12-0055 (Ada 2022 feature)

* *AI12-0055 All properties of a usage profile are defined by pragmas (2020-06-09)*

  AI12-0055 allows the use of the No_Dynamic_CPU_Assignment restriction in pragmas Restrictions and Restrictions_Warnings.

  RM references: D.07 (10/3) D.13 (6/3) D.13 (8/3) D.13 (10/3)

.. index:: AI12-0059 (Ada 2022 feature)

* *AI12-0059 Object_Size attribute (2019-12-02)*

  AI12-0059 brings GNAT-defined attribute Object_Size to Ada standard
  and clarifies its semantics. Given that the attribute already existed in
  GNAT compiler, the feature is supported for all language versions.

  RM references: 4.09.01 (2/3) 13.01 (14) 13.01 (23) 13.03 (9/3) 13.03
  (50/2) 13.03 (51) 13.03 (52) 13.03 (58)

.. index:: AI12-0061 (Ada 2022 feature)

* *AI12-0061 Iterated component associations in array aggregates (2016-09-01)*

  Ada issue AI12-061 introduces a new construct in array aggregates allowing
  component associations to be parameterized by a loop variable, for example:

  .. code::

    Array (1 .. 10) of Integer :=
      (for I in 1 .. 10 => I ** 2);
    type Matrix is
    array
      (Positive range <>, Positive range <>) of Float;
    G : constant Matrix
    :=
      (for I in 1 .. 4 =>
          (for J in 1 .. 4 =>
              (if I=J then
    1.0 else 0.0))); -- Identity matrix

  The expression in such an association can also be a function that returns a
  limited type, and the range can be specified by the 'others' choice.

  RM references: 4.03.03 (5/2) 4.03.03 (6) 4.03.03 (17/3) 4.03.03 (20)
  4.03.03 (23.1/4) 4.03.03 (32/3) 4.03.03 (43) 3.01 (6/3) 3.03 (6)
  3.03 (18.1/3) 3.03.01 (23/3) 5.05 (6) 8.01 (2.1/4)

.. index:: AI12-0062 (Ada 2022 feature)

* *AI12-0062 Raise exception with failing string function (0000-00-00)*

  Clarify that if raising exception E1 is accompanied with a String-valued
  expression whose evaluation raises exception E2, then E2 is what gets propagated.

  RM references: 11.03 (4/2)

.. index:: AI12-0065 (Ada 2022 feature)

* *AI12-0065 Descendants of incomplete views (0000-00-00)*

  This AI is a clarification of potentially confusing wording.  GNAT correctly handles the example given in AARM 7.3.1(5.b-5.d), which illustrates the topic of this AI.

  RM references: 7.03.01 (5.2/3)

.. index:: AI12-0067 (Ada 2022 feature)

* *AI12-0067 Accessibility level of explicitly aliased parameters of procedures and entries (0000-00-00)*

  The AI fixes a case where the intent was fairly obvious but the RM wording failed to mention a case (with the result that the accessibility level of an explicitly aliased parameter of a procedure or entry was undefined even though the intent was clear).

  RM references: 3.10.02 (7/3)

.. index:: AI12-0068 (Ada 2022 feature)

* *AI12-0068 Predicates and the current instance of a subtype (2020-05-06)*

  AI12-0068 is a binding interpretation that defines the current instance name in a type or subtype aspect to be a value rather than an object. This affects
  attributes whose prefix is a current instance in predicates, type invariants, and ``Default_Initial_Condition`` aspects. In particular, in the case of the ``Constrained`` attribute the value will always be True, and formerly legal attributes that require an object as their prefix (such as ``Size``, ``Access``, ``Address``, etc.) are illegal when applied to a current instance in type and subtype aspects.

  RM references: 8.06 (17/3)

.. index:: AI12-0069 (Ada 2022 feature)

* *AI12-0069 Inconsistency in Tree container definition (0000-00-00)*

  The description of how iteration over a Tree container's elements was contradictory in some cases regarding whether a cursor designating the Root node is included in the iteration. This contradiction was resolved. In the "!ACATS Test" section of the AI, it says that if an implementation were to get this wrong then almost any attempt to iterate over any tree would fail at runtime.

  RM references: A.18.10 (153/3) A.18.10 (155/3) A.18.10 (157/3) A.18.10 (159/3)

.. index:: AI12-0070 (Ada 2022 feature)

* *AI12-0070 9.3(2) does not work for anonymous access types (0000-00-00)*

  The RM contained some old wording about the master of an allocated object that only made sense for named access types. The AI clarifies the wording to clearly state the scope of validity and ensures that the paragraph does not contradict 3.10.2's rules for anonymous access  types.

  RM references: 3.10.02 (13.1/3) 9.03 (2)

.. index:: AI12-0071 (Ada 2022 feature)

* *AI12-0071 Order of evaluation when multiple predicates apply (2015-08-10)*

  AI12-0071 specifies the semantics of multiple/inherited predicates on a
  single subtype.

  RM references: 3.02.04 (4/3) 3.02.04 (6/3) 3.02.04 (30/3) 3.02.04 (31/3)
  3.02.04 (32/3) 3.02.04 (33/3) 3.02.04 (35/3) 3.05.05 (7.1/3)
  3.05.05 (7.2/3) 3.05.05 (7.3/3) 3.08.01 (10.1/3) 3.08.01 (15/3)
  4.05.02 (29/3) 4.05.02 (30/3) 4.06 (51/3) 4.09.01 (10/3) 5.04
  (7/3) 5.05 (9/3) 13.09.02 (3/3) 13.09.02 (12)

.. index:: AI12-0072 (Ada 2022 feature)

* *AI12-0072 Missing rules for Discard_Names aspect (0000-00-00)*

  Clarify that Discard_Names is an aspect, not just a pragma.

  RM references: C.05 (1) C.05 (5) C.05 (7/2) C.05 (8)

.. index:: AI12-0073 (Ada 2022 feature)

* *AI12-0073 Synchronous Barriers are not allowed with Ravenscar (2020-02-24)*

  Ada 2022 adds (as a binding interpretation) a ``No_Dependence =>
  Ada.Synchronous_Barriers`` restriction to the Ravenscar profile.

  RM references: D.13 (6/3)

.. index:: AI12-0074 (Ada 2022 feature)

* *AI12-0074 View conversions and out parameters passed by copy (2020-03-26)*

  This Ada 2022 AI makes illegal some cases of out parameters whose type has a
  ``Default_Value`` aspect.

  RM references: 4.06 (56) 6.04.01 (6.25/3) 6.04.01 (13.1/3)

.. index:: AI12-0075 (Ada 2022 feature)

* *AI12-0075 Static expression functions (2020-04-13)*

  Ada 2022 defines a new aspect ``Static`` that can be specified on expression
  functions. Such an expression function can be called in contexts requiring static expressions when the actual parameters are all static, allowing for greater abstraction in complex static expressions.

  RM references: 4.09 (21) 6.08 (3/4) 6.08 (5/4) 6.08 (6/4) 7.03.02 (8.2/5)
  7.03.02 (15/4) 7.03.02 (16/4) 7.03.02 (17/4) 7.03.02 (19/4)
  7.03.02 (20/5)

.. index:: AI12-0076 (Ada 2022 feature)

* *AI12-0076 Variable state in pure packages (0000-00-00)*

  Defines an obscure constant-modifying construct to be erroneous. The issue is that the current instance of a type is a variable object, so the following is legal:

  .. code::

     type T;
     type T_Ref (Access_To_Variable : access T) is null record;
     type T is limited record
        Self : T_Ref (T'Access);
        Int : Integer;
     end record;

     Obj : constant T := (Self => <>, Int => 123);
   begin
     Obj.Self.Access_To_Variable.Int := 456; -- modifying a component of a constant

  In cases where constancy is really needed (e.g., for an object declared in a Pure context), such a case needs to be erroneous.

  RM references: 10.02.01 (17/3) E.02.02 (17/2)

.. index:: AI12-0077 (Ada 2022 feature)

* *AI12-0077 Has_Same_Storage on objects of size zero (2020-03-30)*

  This binding interpretation requires the Has_Same_Storage attribute
  to return always `false` for objects that have a size of zero.

  RM references: 13.03 (73.4/3)

.. index:: AI12-0078 (Ada 2022 feature)

* *AI12-0078 Definition of node for tree container is confusing (0000-00-00)*

  Clarifies the expected behavior in processing tree containers.

  RM references: A.18.10 (2/3) A.18.10 (3/3)

.. index:: AI12-0081 (Ada 2022 feature)

* *AI12-0081 Real-time aspects need to specify when they are evaluated (0000-00-00)*

  Clarify the point at which Priority and Interrupt_Priority aspect expressions are evaluated.

  RM references: D.01 (17/3) D.16 (9/3)

.. index:: AI12-0084 (Ada 2022 feature)

* *AI12-0084 Box expressions in array aggregates (2014-12-15)*

  This AI addresses an issue where compiler used to fail to initialize
  components of a multidimensional aggregates with box initialization when
  scalar components have a specified default value. The AI clarifies that
  in an array aggregate with box (i.e., ``<>``) component values, the
  ``Default_Component_Value`` of the array type (if any) should not be ignored.

  RM references: 4.03.03 (23.1/2)

.. index:: AI12-0085 (Ada 2022 feature)

* *AI12-0085 Missing aspect cases for Remote_Types (0000-00-00)*

  A distributed systems annex (Annex E) clarification. Aspect specifications
  that are forbidden using attribute definition clause syntax are also forbidden
  using aspect_specification syntax.

  RM references: E.02.02 (17/2)

.. index:: AI12-0086 (Ada 2022 feature)

* *AI12-0086 Aggregates and variant parts (2019-08-14)*

  In Ada 2012, a discriminant value that governs an active variant part in an
  aggregate had to be static. AI12-0086 relaxes this restriction: If the subtype of the discriminant value is a static subtype all of whose values select the same variant, then the expression for the discriminant is allowed to be nonstatic.

  RM references: 4.03.01 (17/3) 4.03.01 (19/3)

.. index:: AI12-0088 (Ada 2022 feature)

* *AI12-0088 UTF_Encoding.Conversions and overlong characters on input (0000-00-00)*

  Clarify that overlong characters are acceptable on input even if we never generate them as output.

  RM references: A.04.11 (54/3) A.04.11 (55/3)

.. index:: AI12-0089 (Ada 2022 feature)

* *AI12-0089 Accessibility rules need to take into account that a generic function is not a (0000-00-00)*

  Fix cases in RM wording where the accessibility rules for a function failed to take into account the fact that a generic function is not a function. For example, a generic function with an explicitly aliased parameter should be able to return references to that parameter in the same ways that a (non-generic) function can. The previous wording did not allow that.

  RM references: 3.10.02 (7/3) 3.10.02 (19.2/3) 3.10.02 (19.3/3) 6.05 (4/3)

.. index:: AI12-0093 (Ada 2022 feature)

* *AI12-0093 Iterator with indefinite cursor (0000-00-00)*

  A clarification that confirms what GNAT is already doing.

  RM references: 5.05.02 (8/3) 5.05.02 (10/3)

.. index:: AI12-0094 (Ada 2022 feature)

* *AI12-0094 An access_definition should be a declarative region (0000-00-00)*

  Fixes wording omission in the RM, confirming that the behaviour of GNAT is
  correct.

  RM references: 8.03 (2) 8.03 (26/3)

.. index:: AI12-0095 (Ada 2022 feature)

* *AI12-0095 Generic formal types and constrained partial views (0000-00-00)*

  Deciding whether an actual parameter corresponding to an explicitly aliased formal parameter is legal depends on (among other things) whether the parameter type has a constrained partial view. The AI clarifies how this compile-time checking works in the case of a generic formal type (assume the best in the spec and recheck each instance, assume the worst in a generic body).

  RM references: 3.10.02 (27.2/3) 4.06 (24.16/2) 6.04.01 (6.2/3) 12.05.01 (15)

.. index:: AI12-0096 (Ada 2022 feature)

* *AI12-0096 The exception raised when a subtype conversion fails a predicate check (0000-00-00)*

  Clarify that the Predicate_Failure aspect works the same in a subtype conversion as in any other context.

  RM references: 4.06 (57/3)

.. index:: AI12-0097 (Ada 2022 feature)

* *AI12-0097 Tag of the return object of a simple return expression (0000-00-00)*

  Clarify wording about the tag of a function result in the case of a simple (i.e. not extended) return statement in a function with a class-wide result type.

  RM references: 6.05 (8/3)

.. index:: AI12-0098 (Ada 2022 feature)

* *AI12-0098 Problematic examples for ATC (0000-00-00)*

  The AI clarifies reference manual examples, there is no compiler impact.

  RM references: 9.07.04 (13)

.. index:: AI12-0099 (Ada 2022 feature)

* *AI12-0099 Wording problems with predicates (2020-05-04)*

  When extending a task or protected type from an ancestor interface subtype with  a predicate, a link error can occur due to the compiler failing to generate the predicate-checking function. This AI clarifies the requirement for such predicate inheritance for concurrent types.

  RM references: 3.02.04 (4/4) 3.02.04 (12/3) 3.02.04 (20/3)

.. index:: AI12-0100 (Ada 2022 feature)

* *AI12-0100 A qualified expression makes a predicate check (2020-02-17)*

  The compiler now enforces predicate checks on qualified expressions when the
  qualifying subtype imposes a predicate.

  RM references: 4.07 (4)

.. index:: AI12-0101 (Ada 2022 feature)

* *AI12-0101 Incompatibility of hidden untagged record equality (2019-10-31)*

  AI12-0101 is a binding interpretation that removes a legality rule that
  prohibited the declaration of a primitive equality function for a private type in the private part of its enclosing package (either before or after the completion of the type) when the type is completed as an untagged record type. Such declarations are now accepted in Ada 2012 and later Ada versions.

  As a consequence of this work, some cases where the implementation of AI05-0123 was incomplete were corrected.
  More specifically, if a user-defined equality operator is present for an untagged record type in an Ada 2012 program, that user-defined equality operator will be (correctly) executed in some difficult-to-characterize cases where the predefined component-by-component comparison was previously being (incorrectly) executed. This can arise, for example, in the case of the predefined equality operation for an enclosing composite type that has a component of the user-defined primitive equality op's operand type.
  This correction means that the impact of this change is not limited solely to code that was previously rejected at compile time.

  RM references: 4.05.02 (9.8/3)

.. index:: AI12-0102 (Ada 2022 feature)

* *AI12-0102 Stream_IO.File_Type has Preelaborable_Initialization (0000-00-00)*

  Modifies the declaration of one type in a predefined package. GNAT's version of ``Ada.Streams.Stream_IO`` already had this modification (the ``Preelaborable__Initialization`` pragma).

  RM references: A.12.01 (5)

.. index:: AI12-0103 (Ada 2022 feature)

* *AI12-0103 Expression functions that are completions in package specifications (0000-00-00)*

  Clarifies that expression functions that are completions do not cause "general"  freeze-everybody-in-sight freezing like a subprogram body.

  RM references: 13.14 (3/3) 13.14 (5/3)

.. index:: AI12-0104 (Ada 2022 feature)

* *AI12-0104 Overriding an aspect is undefined (0000-00-00)*

  A clarification of the wording in RM, no compiler impact.

  RM references: 4.01.06 (4/3) 4.01.06 (17/3)

.. index:: AI12-0105 (Ada 2022 feature)

* *AI12-0105 Pre and Post are not allowed on any subprogram completion (0000-00-00)*

  Language-defined aspects (e.g., ``Post``) cannot be specified as part of the completion of  a subprogram declaration. Fix a hole in the RM wording to clarify that this general rule applies even in the special cases where the completion is either an expression function or a null procedure.

  RM references: 13.01.01 (18/3)

.. index:: AI12-0106 (Ada 2022 feature)

* *AI12-0106 Write'Class aspect (0000-00-00)*

  Clarify that the syntax used in an ACATS test BDD2005 for specifying a class-wide streaming aspect is correct.

  RM references: 13.01.01 (28/3) 13.13.02 (38/3)

.. index:: AI12-0107 (Ada 2022 feature)

* *AI12-0107 A prefixed view of a By_Protected_Procedure interface has convention protected (2020-06-05)*

  A prefixed view of a subprogram with aspect Synchronization set to
  By_Protected_Procedure has convention protected.

  RM references: 6.03.01 (10.1/2) 6.03.01 (12) 6.03.01 (13)

.. index:: AI12-0109 (Ada 2022 feature)

* *AI12-0109 Representation of untagged derived types (2019-11-12)*

  Ada disallows a nonconforming specification of a type-related representation
  aspect of an untagged by-reference type. The motivation for this rule is to ensure that a parent type and a later type derived from the parent agree with respect to such aspects. AI12-0109 disallows a construct that otherwise could be used to get around this rule: an aspect specification for the parent type that occurs after the declaration of the derived type.

  RM references: 13.01 (10/3)

.. index:: AI12-0110 (Ada 2022 feature)

* *AI12-0110 Tampering checks are performed first (2020-04-14)*

  AI12-0110 requires tampering checks in the containers library to be
  performed first, before any other checks.

  RM references: A.18.02 (97.1/3) A.18.03 (69.1/3) A.18.04 (15.1/3) A.18.07
  (14.1/3) A.18.10 (90/3) A.18.18 (35/3)

.. index:: AI12-0112 (Ada 2022 feature)

* *AI12-0112 Contracts for container operations (0000-00-00)*

  A representation change replacing english descriptions of contracts for
  operations on predefined container types with pre/post-conditions. No compiler
  impact.

  RM references: A.18.02 (99/3) 11.04.02 (23.1/3) 11.05 (23) 11.05 (26) A
  (4) A.18 (10)

.. index:: AI12-0114 (Ada 2022 feature)

* *AI12-0114 Overlapping objects designated by access parameters are not thread-safe (0000-00-00)*

  There are rules saying that concurrent calls to predefined subprograms don't interfere with each other unless actual parameters overlap. The AI clarifies that such an interference is also possible if overlapping objects are reachable via access dereferencing from actual parameters of the two calls.

  RM references: A (3/2)

.. index:: AI12-0116 (Ada 2022 feature)

* *AI12-0116 Private types and predicates (0000-00-00)*

  Clarify that the same aspect cannot be specified twice for the same type. ``Dynamic_Predicate``, for example, can be specified on either the partial view of a type or on the completion in the private part, but not on both.

  RM references: 13.01 (9/3) 13.01 (9.1/3)

.. index:: AI12-0117 (Ada 2022 feature)

* *AI12-0117 Restriction No_Tasks_Unassigned_To_CPU (2020-06-12)*

  This AI adds a restriction No_Tasks_Unassigned_To_CPU to provide safe
  use of Ravenscar.

  The CPU aspect is specified for the environment task. No CPU aspect is
  specified to be statically equal to ``Not_A_Specific_CPU``. If aspect CPU
  is specified (dynamically) to the value ``Not_A_Specific_CPU``, then
  Program_Error is raised. If Set_CPU or ``Delay_Until_And_Set_CPU`` are called
  with the CPU parameter equal to ``Not_A_Specific_CPU``, then ``Program_Error`` is raised.

  RM references: D.07 (10.8/3)

.. index:: AI12-0120 (Ada 2022 feature)

* *AI12-0120 Legality and exceptions of generalized loop iteration (0000-00-00)*

  Clarify that the expansion-based definition of generalized loop iteration
  includes legality checking. If the expansion would be illegal (for example,
  because of passing a constant actual parameter in a call when the mode of
  the corresponding formal parameter is in-out), then the loop is illegal too.

  RM references: 5.05.02 (6.1/4) 5.05.02 (10/3) 5.05.02 (13/3)

.. index:: AI12-0121 (Ada 2022 feature)

* *AI12-0121 Stream-oriented aspects (0000-00-00)*

  Clarify that streaming-oriented aspects (e.g., Read) can be specified using
  aspect_specification syntax, not just via an attribute definition clause.

  RM references: 13.13.02 (38/3)

.. index:: AI12-0124 (Ada 2022 feature)

* *AI12-0124 Add Object'Image (2017-03-24)*

  The corrigendum of Ada 2012 extends attribute ``'Image following`` the syntax for the GNAT ``'Img`` attribute. This AI fixes a gap in the earlier implementation, which did not recognize function calls and attributes that are functions as valid object prefixes.

  RM references: 3.05 (55/3)

.. index:: AI12-0125-3 (Ada 2022 feature)

* *AI12-0125-3 Add @ as an abbreviation for the LHS of an assignment (2016-11-11)*

  This AI introduces the use of the character '@' as an abbreviation for the left-hand side of an assignment statement, usable anywhere within the expression on the right-hand side. To use this feature the compilation flag -gnat2022 must be specified.

  RM references: 5.02.01 (0) 2.02 (9) 3.03 (21.1/3) 4.01 (2/3) 8.06 (9/4)

.. index:: AI12-0127 (Ada 2022 feature)

* *AI12-0127 Partial aggregate notation (2016-10-12)*

  This AI describes a new constructor for aggregates, in terms of an existing record or array object, and a series of component-wise modifications of its value, given by named associations for the modified components. To use this feature the compilation flag ``-gnat2022`` must be specified.

  RM references: 4.03 (2) 4.03 (3/2) 4.03 (4) 4.03.01 (9) 4.03.01 (15/3)
  4.03.01 (16/4) 4.03.01 (17/5) 4.03.01 (17.1/2) 4.03.03 (4) 4.03.03
  (14) 4.03.03 (17/5) 4.03.04 (0) 7.05 (2.6/2)

.. index:: AI12-0128 (Ada 2022 feature)

* *AI12-0128 Exact size access to parts of composite atomic objects (2019-11-24)*

  According to this AI, the compiler generates full access to atomic composite objects even if the access is only partial in the source code. To use this feature the compilation flag ``-gnat2022`` must be specified.

  RM references: C.06 (13.2/3) C.06 (19) C.06 (20) C.06 (22/2) C.06 (25/4)

.. index:: AI12-0129 (Ada 2022 feature)

* *AI12-0129 Make protected objects more protecting (2020-07-01)*

  A new aspect Exclusive_Functions has been added to the language to force the
  use of read/write locks on protected functions when needed.

  RM references: 9.05.01 (2) 9.05.01 (4) 9.05.01 (5) 9.05.01 (7) 9.05.03
  (15) 9.05.03 (23)

.. index:: AI12-0130 (Ada 2022 feature)

* *AI12-0130 All I/O packages should have Flush (2016-07-03)*

  The Flush routine has been added for the ``Sequential_IO`` and ``Direct_IO`` standard packages in the Ada 2012 COR.1:2016. The Flush routine here is equivalent to the one found in ``Text_IO``. The ``Flush`` procedure synchronizes the external file with the internal file (by flushing any internal buffers) without closing the file.

  RM references: A.08.01 (10) A.08.02 (28/3) A.08.04 (10) A.10.03 (21)
  A.12.01 (28/2) A.12.01 (28.6/1)

.. index:: AI12-0131 (Ada 2022 feature)

* *AI12-0131 Inherited Pre'Class when unspecified on initial subprogram (0000-00-00)*

  If T1 is a tagged type with a primitive P that has no class-wide precondition,
  and if T2 is an extension of T1 which overrides the inherited primitive P, then that overriding P is not allowed to have a class-wide precondition. Allowing it would be ineffective except in corner cases where it would be confusing.

  RM references: 6.01.01 (17/3) 6.01.01 (18/3)

.. index:: AI12-0132 (Ada 2022 feature)

* *AI12-0132 Freezing of renames-as-body (2020-06-13)*

  This AI clarifies that a renames-as-body freezes the expression of any
  expression function that it renames.

  RM references: 13.14 (5/3)

.. index:: AI12-0133 (Ada 2022 feature)

* *AI12-0133 Type invariants and default initialized objects (0000-00-00)*

  Clarify that invariant checking for a default-initialized object is performed regardless of where the object is declared (in particular, even when the full view of the type is visible).

  RM references: 7.03.02 (10.3/3)

.. index:: AI12-0135 (Ada 2022 feature)

* *AI12-0135 Enumeration types should be eligible for convention C (0000-00-00)*

  Ada previously allowed but did not require supporting specifying convention C for an enumeration type. Now it is required that an implementation shall support it.

  RM references: B.01 (14/3) B.01 (41/3) B.03 (65)

.. index:: AI12-0136 (Ada 2022 feature)

* *AI12-0136 Language-defined packages and aspect Default_Storage_Pool (0000-00-00)*

  Clarify that the effect of specifying Default_Storage_Pool for an instance of a predefined generic is implementation-defined. No compiler impact.

  RM references: 13.11.03 (5/3)

.. index:: AI12-0137 (Ada 2022 feature)

* *AI12-0137 Incomplete views and access to class-wide types (0000-00-00)*

  If the designated type of an access type is incomplete when the access type is declared, then we have rules about whether we get a complete view when a value of the access type is dereferenced. Clarify that analogous rules apply if the designated type is class-wide.

  RM references: 3.10.01 (2.1/2)

.. index:: AI12-0138 (Ada 2022 feature)

* *AI12-0138 Iterators of formal derived types (2021-02-11)*

  AI12-0138 specifies the legality rules for confirming specifications of
  nonoverridable aspects. This completes the legality checks for aspect ``Implicit_Dereference`` and simplifies the checks for those aspects that are inherited operations.

  RM references: 13.01.01 (18/4) 13.01.01 (34/3) 4.01.05 (6/3) 4.01.06 (5/3)
  4.01.06 (6/3) 4.01.06 (7/3) 4.01.06 (8/3) 4.01.06 (9/3) 5.05.01 (11/3)

.. index:: AI12-0140 (Ada 2022 feature)

* *AI12-0140 Access to unconstrained partial view when full view is constrained (0000-00-00)*

  Clarify some confusion about about whether what matters when checking whether designated subtypes statically match is the view of the designated type that is currently available v.s. the view that was available when the access type was declared.

  RM references: 3.02 (7/2) 7.03.01 (5/1)

.. index:: AI12-0143 (Ada 2022 feature)

* *AI12-0143 Using an entry index of a family in a precondition (2022-04-05)*

  Ada 2022 adds the ``Index`` attribute, which allows the use of the entry family index of an entry call within preconditions and post-conditions.

  RM references: 6.01.01 (30/3) 9.05.04 (5/3)

.. index:: AI12-0144 (Ada 2022 feature)

* *AI12-0144 Make Discrete_Random more flexible (2020-01-31)*

  A new function Random with First/Last parameters is provided in the
  ``Ada.Numerics.Discrete_Random`` package.

  RM references: A.05.02 (20) A.05.02 (32) A.05.02 (41) A.05.02 (42)

.. index:: AI12-0145 (Ada 2022 feature)

* *AI12-0145 Pool_of_Subpool returns null when called too early (0000-00-00)*

  Clarify that if you ask for the pool of a subpool (by calling ``Pool_Of_Subpool``) before ``Set_Pool_of_Subpool`` is called, then the result is null.

  RM references: 13.11.04 (20/3)

.. index:: AI12-0147 (Ada 2022 feature)

* *AI12-0147 Expression functions and null procedures can be declared in a protected_body (2015-03-05)*

  AI12-0147 specifies that null procedures and expression functions are now
  allowed in protected bodies.

  RM references: 9.04 (8/1)

.. index:: AI12-0149 (Ada 2022 feature)

* *AI12-0149 Type invariants are checked for functions returning access-to-type (0000-00-00)*

  Extend the rule saying that ``Type_Invariant`` checks are performed for access-to-T parameters (where T has a specified ``Type_Invariant``) so that the rule also applies to function results.

  RM references: 7.03.02 (19.3/4)

.. index:: AI12-0150 (Ada 2022 feature)

* *AI12-0150 Class-wide type invariants and statically bound calls (0000-00-00)*

  The same approach used in AI12-0113 to ensure that contract-related calls associated with a call to a subprogram "match" with respect to dispatching also applies to ``Type_Invariant`` checking.

  RM references: 7.03.02 (3/3) 7.03.02 (5/3) 7.03.02 (9/3) 7.03.02 (22/3)

.. index:: AI12-0154 (Ada 2022 feature)

* *AI12-0154 Aspects of library units (0000-00-00)*

  Clarify that an aspect_specification for a library unit is equivalent to a corresponding aspect-specifying pragma.

  RM references: 13.01.01 (32/3)

.. index:: AI12-0156 (Ada 2022 feature)

* *AI12-0156 Use subtype_indication in generalized iterators (0000-00-00)*

  For iterating over an array, we already allow (but do not require) explicitly providing a subtype indication in an iterator_specification. Tee AI generalizes this to handle the case where the element type of the array is of an anonymous access type. This also allows (but does not require) explicitly naming the cursor subtype in a generalized iterator.
  The main motivation for allowing these new cases is improving readability by making it easy to infer the (sub)type of the iteration object just by looking at the loop.

  RM references: 5.05.02 (2/3) 5.05.02 (5/4) 5.05.02 (7/3) 3.10.02 (11.1/2)

.. index:: AI12-0157 (Ada 2022 feature)

* *AI12-0157 Missing rules for expression functions (0000-00-00)*

  Clarify that an expression function behaves like a single-return-statement
  function in more cases: it can return an aggregate without extra parens, the expression has an applicable index constraint, and the same accessibility rules apply in both cases.

  For instance, the code below is legal:

  .. code::

    subtype S is String (1 .. 10);
    function f return S is (others => '?');

  RM references: 3.10.02 (19.2/4) 3.10.02 (19.3/4) 4.03.03 (11/2) 6.08 (2/3)
  6.08 (3/3) 6.08 (5/3) 6.08 (6/3) 6.08 (7/3) 7.05 (2.9/3) 13.14
  (5.1/4) 13.14 (5.2/4) 13.14 (8/3) 13.14 (10.1/3) 13.14 (10.2/3)
  13.14 (10.3/3)

.. index:: AI12-0160 (Ada 2022 feature)

* *AI12-0160 Adding an indexing aspect to an indexable container type (0000-00-00)*

  If the parent type of a derived type has exactly one of the two indexing aspects (that is, constant_indexing and variable_indexing) specified, then the derived type cannot have a specification for the other one.

  RM references: 4.01.06 (6/4) 4.01.06 (9/4) 3.06 (22.2/3)

.. index:: AI12-0162 (Ada 2022 feature)

* *AI12-0162 Memberships and Unchecked_Unions (0000-00-00)*

  Clarify that membership tests for unchecked_union types work consistently when
  testing membership in more than one subtype (X in AA | BB | CC)  as when
  testing for one.

  RM references: B.03.03 (25/2)

.. index:: AI12-0164 (Ada 2022 feature)

* *AI12-0164 Max_Entry_Queue_Length aspect for entries (2019-06-11)*

  AI12-0164 defines pragma and aspect ``Max_Entry_Queue_Length`` in addition
  to the GNAT-specific equivalents ``Max_Queue_Length`` and ``Max_Entry_Queue_Depth``.

  RM references: D.04 (16)

.. index:: AI12-0165 (Ada 2022 feature)

* *AI12-0165 Operations of class-wide types and formal abstract subprograms (2021-10-19)*

  Ada 2022 specifies that when the controlling type of a formal abstract
  subprogram declaration is a formal type, and the actual type is a class-wide type T'Class, the actual subprogram can be an implicitly declared subprogram corresponding to a primitive operation of type T.

  RM references: 12.06 (8.5/2)

.. index:: AI12-0166 (Ada 2022 feature)

* *AI12-0166 External calls to protected functions that appear to be internal calls (2016-11-15)*

  According to this AI, the compiler rejects a call to a protected operation when the call appears within a precondition for another protected operation.

  RM references: 6.01.01 (34/3) 9.05 (3/3) 9.05 (7.1/3)

.. index:: AI12-0167 (Ada 2022 feature)

* *AI12-0167 Type_Invariants and tagged-type View Conversions (0000-00-00)*

  This AI clarifies that no invariant check is performed in a case where an invariant-violating value is assigned to a component. This confirms the current compiler behavior.

  RM references: 7.03.02 (9/4)

.. index:: AI12-0168 (Ada 2022 feature)

* *AI12-0168 Freezing of generic instantiations of generics with bodies (0000-00-00)*

  Adjust freezing rules to be compatible with AI12-0103-1. The change confirms the current compiler behavior.

  RM references: 13.14 (3/4)

.. index:: AI12-0169 (Ada 2022 feature)

* *AI12-0169 Aspect specifications for entry bodies (0000-00-00)*

  Change syntax to allow aspect specifications for implementation-defined aspects on entry bodies. The change doesn't influence any of the language-defined aspects and is solely required for SPARK.

  RM references: 9.05.02 (5)

.. index:: AI12-0170 (Ada 2022 feature)

* *AI12-0170 Abstract subprogram calls in class-wide precondition expressions (2020-07-06)*

  This AI specifies rules for calls to abstract functions within class-wide preconditions and postconditions.

  RM references: 3.09.03 (7) 6.01.01 (7/4) 6.01.01 (18/4) 6.01.01 (18.2/4)

.. index:: AI12-0172 (Ada 2022 feature)

* *AI12-0172 Raise expressions in limited contexts (2019-07-29)*

  The compiler has been enhanced to support the use of raise expressions in
  limited contexts.

  RM references: 7.05 (2.1/3)

.. index:: AI12-0173 (Ada 2022 feature)

* *AI12-0173 Expression of an extended return statement (0000-00-00)*

  Fix the wording related to expression of an extended return statement that was made ambiguous by changes of syntax in other AI's. No compiler changes involved.

  RM references: 6.05 (3/2) 6.05 (5/3)

.. index:: AI12-0174 (Ada 2022 feature)

* *AI12-0174 Aggregates of Unchecked_Unions using named notation (0000-00-00)*

  In many cases, it is illegal to name a discriminant of an unchecked_union type.  Relax this rule to allow the use of named notation in an aggregate of an unchecked_union type.

  RM references: B.03.03 (9/3)

.. index:: AI12-0175 (Ada 2022 feature)

* *AI12-0175 Preelaborable packages with address clauses (2020-03-20)*

  The compiler nows accepts calls to certain functions that are essentially unchecked conversions in preelaborated library units. To use this feature the compilation flag ``-gnat2022`` must be specified.

  RM references: 10.02.01 (7)

.. index:: AI12-0179 (Ada 2022 feature)

* *AI12-0179 Failure of postconditions of language-defined units (0000-00-00)*

  A clarification that expressing postconditions for predefined units via RM wording or via ``Post`` aspect specifications are equivalent. In particular, the expression in such a ``Post`` aspect specification should not yield False. No implementation changes needed.

  RM references: 1.01.03 (17/3) 11.04.02 (23.1/3)

.. index:: AI12-0180 (Ada 2022 feature)

* *AI12-0180 Using protected subprograms and entries within an invariant (2020-06-22)*

  AI12-0180 makes entries and protected subprograms directly visible within Invariant aspects of a task or protected type.

  RM references: 13.01.01 (12/3)

.. index:: AI12-0181 (Ada 2022 feature)

* *AI12-0181 Self-referencing representation aspects (0000-00-00)*

  Clarify that a name or expression which freezes an entity cannot occur in an aspect specification for that entity.

  RM references: 13.01 (9/4) 13.01 (9.1/4) 13.14 (19)

.. index:: AI12-0182 (Ada 2022 feature)

* *AI12-0182 Pre'Class and protected operations (0000-00-00)*

  Confirm that Pre'Class and Post'Class cannot be specified for a protected operation. No language change.

  RM references: 13.01.01 (16/3)

.. index:: AI12-0184 (Ada 2022 feature)

* *AI12-0184 Long Long C Data Types (2020-01-30)*

  Two new types ``long_long`` and ``unsigned_long_long`` are introduced in the package ``Interfaces.C``.

  RM references: B.03 (71.3/3)

.. index:: AI12-0185 (Ada 2022 feature)

* *AI12-0185 Resolution of postcondition-specific attributes (0000-00-00)*

  Clarify resolution rules for ``'Old`` and ``'Result`` attribute references to match original intent.

  RM references: 6.01.01 (7/4) 6.01.01 (8/3) 6.01.01 (26.10/4) 6.01.01 (29/3)

.. index:: AI12-0186 (Ada 2022 feature)

* *AI12-0186 Profile freezing for the Access attribute (0000-00-00)*

  Clarify that the use of Some_Subprogram'Access does not freeze the profile of Some_Subprogram.

  RM references: 13.14 (15)

.. index:: AI12-0187 (Ada 2022 feature)

* *AI12-0187 Stable properties of abstract data types (2020-11-04)*

  Ada 2022 defines a new aspect, ``Stable_Properties``, for use in
  generating additional postcondition checks for subprograms.

  RM references: 7.03.04 (0) 13.01.01 (4/3)

.. index:: AI12-0191 (Ada 2022 feature)

* *AI12-0191 Clarify "part" for type invariants (0000-00-00)*

  Clarify that for purposes of determining whether an invariant check is required for a "part" of an object, we do not look at "parts" which do not correspond to "parts" of the nominal type of the object. For example, if we have a parameter Param of a tagged type T1 (or equivalently of type T1'Class), and type T2 is an extension of T1 which declares a component Foo, and T1'Class (Param)'Tag = T2'Tag, then no invariant check is performed for Param's Foo component (or any subcomponent thereof).

  RM references: 3.03 (23/5) 3.09.01 (4.1/2) 6.08 (5.8/5) 7.03.02 (8.3/5)
  7.03.02 (8.4/5) 7.03.02 (8.5/5) 7.03.02 (8.6/5) 7.03.02 (8.7/5)
  7.03.02 (8.8/5) 7.03.02 (8.9/5) 7.03.02 (8.10/5) 7.03.02 (8.11/5)
  7.03.02 (8.12/5) 7.03.02 (10.1/4) 7.03.02 (15/5) 7.03.02 (17/4)
  7.03.02 (18/4) 7.03.02 (19/4) 13.13.02 (9/3)

.. index:: AI12-0192 (Ada 2022 feature)

* *AI12-0192 "requires late initialization" and protected types (2020-03-11)*

  This AI clarifies that components of a protected type require late initialization when their initialization references (implicitly) the current instance of the type.

  RM references: 3.03.01 (8.1/2)

.. index:: AI12-0194 (Ada 2022 feature)

* *AI12-0194 Language-defined aspects and entry bodies (0000-00-00)*

  The AI Includes entry bodies on the list of bodies for which no language-defined aspects can be specified (although specifying an implementation-defined aspect may be allowed).

  A wording change, no implementation impact.

  RM references: 13.01.01 (17/3)

.. index:: AI12-0195 (Ada 2022 feature)

* *AI12-0195 Inheriting body but overriding precondition or postcondition (2021-08-11)*

  Ada 2022 specifies that if a primitive with a class-wide precondition or
  postcondition is inherited, and some primitive function called in the class-wide precondition or postcondition is overridden, then a dispatching call to the first primitive with a controlling operand that has the tag of the overriding type is required to check both the interpretation using the overriding function and the interpretation using the original overridden function.

  RM references: 6.01.01 (38/4)

.. index:: AI12-0196 (Ada 2022 feature)

* *AI12-0196 Concurrent access to Ada container libraries (0000-00-00)*

  Clarify that parallel execution of operations which use cursors to refer to different elements of the same container does not violate the rules about erroneous concurrent access in some cases. That is, if C1 and C2 are cursors that refer to different elements of some container, then it is ok to concurrently execute an operation that is passed C1 and which accesses one element of the container, with another operation (perhaps the same operation, perhaps not) that is passed C2 and which accesses another element of the container.

  RM references: A.18 (2/2) A.18.02 (125/2) A.18.02 (133/3) A.18.02 (135/3)
  A.18.03 (81/3) A.18.04 (36/3) A.18.07 (34/2) A.18.10 (116/3)

.. index:: AI12-0198 (Ada 2022 feature)

* *AI12-0198 Potentially unevaluated components of array aggregates (2020-05-13)*

  Ada 2022 enforces the detection of components that belong to a nonstatic or
  null range of index values of an array aggregate.

  RM references: 6.01.01 (22.1/4)

.. index:: AI12-0199 (Ada 2022 feature)

* *AI12-0199 Abstract subprogram calls in class-wide invariant expressions (0000-00-00)*

  Class-wide type invariants do not apply to abstract types, to avoid various
  problems. Define the notion of a "corresponding expression" for a class-wide
  type invariant, replacing references to components as appropriate, taking into
  account rules for corresponding and specified discriminants when applying them
  to a nonabstract descendant.

  RM references: 7.03.02 (5/4) 7.03.02 (8/3)

.. index:: AI12-0201 (Ada 2022 feature)

* *AI12-0201 Missing operations of static string types (2020-02-25)*

  Relational operators and type conversions of static string types are now static in Ada 2022.

  RM references: 4.09 (9) 4.09 (19) 4.09 (20) 4.09 (24)

.. index:: AI12-0203 (Ada 2022 feature)

* *AI12-0203 Overriding a nonoverridable aspect (0000-00-00)*

  A corner case wording clarification that has no impact on compilers.

  RM references: 4.01.05 (5.1/4) 4.01.05 (7/3)

.. index:: AI12-0204 (Ada 2022 feature)

* *AI12-0204 Renaming of a prefixed view (2020-02-24)*

  AI12-0204 clarifies that the prefix of a prefixed view that is renamed or
  passed as a formal subprogram must be renameable as an object.

  RM references: 8.05.04 (5.2/2) 12.06 (8.3/2) 4.01.03 (13.1/2) 4.01.06 (9/5)

.. index:: AI12-0205 (Ada 2022 feature)

* *AI12-0205 Defaults for generic formal types (2021-04-01)*

  AI12-0205 specifies syntax and semantics that provide defaults for formal types of generic units. The legality rules guarantee that the default subtype_mark that is specified for a formal type would be a legal actual in any instantiation of the generic unit.

  RM references: 12.03 (7/3) 12.03 (10) 12.05 (2.1/3) 12.05 (2.2/3) 12.05 (7/2)

.. index:: AI12-0206 (Ada 2022 feature)

* *AI12-0206 Nonoverridable should allow arbitrary kinds of aspects (0000-00-00)*

  A non-overridable aspect can have a value other than a name; for example, ``Max_Entry_Queue_Length`` is non-overridable and it has a scalar value.
  Part of adding support for ``Max_Entry_Queue_Length`` (which is already supported by GNAT).

  RM references: 13.01.01 (18.2/4) 13.01.01 (18.3/4) 13.01.01 (18.6/4)

.. index:: AI12-0207 (Ada 2022 feature)

* *AI12-0207 Convention of anonymous access types (2020-02-01)*

  The convention of anonymous access elements of arrays now have the same convention as the array instead of convention Ada.

  RM references: 6.03.01 (13.1/3) B.01 (19) B.01 (21/3)

.. index:: AI12-0208 (Ada 2022 feature)

* *AI12-0208 Predefined Big numbers support (0000-00-00)*

  Add predefined package ``Ada.Numerics.Big_Numbers``.

  RM references: A.05.05 (0) A.05.06 (0) A.05.07 (0)

.. index:: AI12-0211 (Ada 2022 feature)

* *AI12-0211 Interface types and inherited nonoverridable aspects (2020-08-24)*

  AI12-0211 introduces two new legality rules for Ada 2022. The first says that
  if a nonoverridable aspect is explicitly specified for a type that also inherits that aspect from another type (an ancestor or a progenitor), then the explicit aspect specification shall be confirming. The second says that if a type inherits a nonoverridable aspect from two different sources (this can only occur if at least one of the two is an interface type), then the two sources shall agree with respect to the given aspect. This AI is a binding interpretation, so these checks are performed even for earlier Ada versions. Because of compatibility concerns, an escape mechanism for suppressing these legality checks is provided: these new checks always pass if the ``-gnatd.M`` switch (relaxed RM semantics) is specified.

  RM references: 13.01.01 (18.3/5) 13.01.01 (18.4/4)

.. index:: AI12-0212 (Ada 2022 feature)

* *AI12-0212 Container aggregates; generalized array aggregates (0000-00-00)*

  The AI defines a new feature: generalized array aggregates that already exists in GNAT.

  RM references: 4.03.05 (0) 1.01.04 (12) 1.01.04 (13) 2.01 (15) 2.02 (9/5)
  3.07.01 (3) 3.08.01 (4) 4.03 (2/5) 4.03 (3/5) 4.03.01 (5) 4.03.03
  (3/2) 4.03.03 (4/5) 4.03.03 (5.1/5) 4.03.03 (9) 4.03.03 (17/5)
  4.03.03 (21) 4.03.03 (23.2/5) 4.03.03 (26) 4.03.03 (27) 4.03.03
  (31) 4.03.04 (4/5) 4.04 (3.1/3) 11.02 (3) 13.01.01 (5/3)
  13.01.01 (7/3) A.18.02 (8/3) A.18.02 (14/2) A.18.02 (47/2) A.18.02
  (175/2) A.18.03 (6/3) A.18.05 (3/3) A.18.06 (4/3) A.18.08 (3/3)
  A.18.09 (4/3)

.. index:: AI12-0216 (Ada 2022 feature)

* *AI12-0216 6.4.1(6.16-17/3) should never apply to composite objects (0000-00-00)*

  Fix wording so that parameter passing cases where there isn't really any aliasing problems or evaluation order dependency are classified as acceptable.

  No compiler impact.

  RM references: 6.04.01 (6.17/3)

.. index:: AI12-0217 (Ada 2022 feature)

* *AI12-0217 Rules regarding restrictions on the use of the Old attribute are too strict (2020-03-25)*

  AI12-0217 loosens the rules regarding what is allowed as the prefix of a 'Old
  attribute reference. In particular, a prefix is now only required to "statically name" (as opposed to the previous "statically denote") an object. This means that components of composite objects that previously would have been illegal are now legal prefixes.

  RM references: 6.01.01 (24/3) 6.01.01 (27/3)

.. index:: AI12-0220 (Ada 2022 feature)

* *AI12-0220 Pre/Post for access-to-subprogram types (2020-04-14)*

  Contract aspects can now be specified for access-to-subprogram types, as
  defined for Ada 2022 in this AI.

  RM references: 6.01.01 (1/4) 6.01.01 (2/3) 6.01.01 (4/3) 6.01.01 (19/3)
  6.01.01 (28/3) 6.01.01 (29/3) 6.01.01 (39/3) 13.01.01 (12/5)

.. index:: AI12-0222 (Ada 2022 feature)

* *AI12-0222 Representation aspects and private types (0000-00-00)*

  Clarify that the rule against specifying a representation aspect for a type before the type is completely defined also applies in the case where aspect_specification syntax is used (not just in the case where a pragma or some other kind of representation item is used).

  GNAT already implements this.

  RM references: 13.01 (9/5) 13.01 (9.1/4) 13.01 (9.2/5)

.. index:: AI12-0225 (Ada 2022 feature)

* *AI12-0225 Prefix of Obj'Image (0000-00-00)*

  Clarify some Object vs. Value corner cases to allow names that do not denote objects in more contexts, such as a qualified expression as a prefix of an Image attribute.

  RM references: 3.05 (55.1/4)

.. index:: AI12-0226 (Ada 2022 feature)

* *AI12-0226 Make objects more consistent (0000-00-00)*

  Allow value conversions as objects. For instance this example becomes legal: ``Long_Integer (Duration'Last)'Image``.

  RM references: 3.03 (11.1/3) 3.03 (21.1/3) 3.03 (23.8/5) 4.06 (58.1/4)
  4.06 (58.3/4)

.. index:: AI12-0227 (Ada 2022 feature)

* *AI12-0227 Evaluation of nonstatic universal expressions when no operators are involved (0000-00-00)*

  Nonstatic universal integer expressions are always evaluated at runtime as values of type root_integer; similarly, nonstatic universal real expressions are always evaluated at runtime as values of type root_real.
  This AI corrects a wording oversight. Previously, the above was only true if a call to operator was involved. With this change it is true in all cases.

  No compiler impact.

  RM references: 4.04 (10) 8.06 (29)

.. index:: AI12-0228 (Ada 2022 feature)

* *AI12-0228 Properties of qualified expressions used as names (2020-02-19)*

  This AI clarifies that properties of a qualified object pass through a
  qualified expression used as a name. Specifically, "aliased" and "known to be
  constrained" are not changed by a qualified expression.

  RM references: 3.03 (23.7/3) 3.10 (9/3)

.. index:: AI12-0231 (Ada 2022 feature)

* *AI12-0231 Null_Task_Id and Activation_Is_Complete (0000-00-00)*

  Add ``Activation_Is_Complete`` to the list of functions that raise P_E if passed ``Null_Task_Id``, correcting an oversight.

  RM references: C.07.01 (15)

.. index:: AI12-0232 (Ada 2022 feature)

* *AI12-0232 Rules for pure generic bodies (0000-00-00)*

  Clarify the rules for a generic body nested in a pure library unit.

  RM references: 10.02.01 (9/3) 10.02.01 (15.1/3) 10.02.01 (15.5/3)

.. index:: AI12-0233 (Ada 2022 feature)

* *AI12-0233 Pre'Class for hidden operations of private types (0000-00-00)*

  Clarify how ``Pre'Class`` checking interacts with private-part overriding of inherited subprograms. A class-wide precondition can be checked at runtime even if it is specified in a private part that the caller cannot see into.

  RM references: 6.01.01 (38/4)

.. index:: AI12-0234 (Ada 2022 feature)

* *AI12-0234 Compare-and-swap for atomic objects (0000-00-00)*

  New predefined units for atomic operations (``System.Atomic_Operations`` and child units thereof).

  RM references: C.06.01 (0) C.06.02 (0)

.. index:: AI12-0235 (Ada 2022 feature)

* *AI12-0235 System.Storage_Pools should be pure (0000-00-00)*

  Change the predefined package System.Storage_Pools from preelaborated to pure.

  RM references: 13.11 (5)

.. index:: AI12-0236 (Ada 2022 feature)

* *AI12-0236 declare expressions (2020-04-08)*

  A ``declare expression`` allows constant objects and renamings to be
  declared within an expression.

  RM references: 2.08 (6) 3.09.02 (3) 3.10.02 (9.1/3) 3.10.02 (16.1/3)
  3.10.02 (32.2/3) 4.03.02 (5.4/3) 4.03.03 (15.1/3) 4.04 (7/3)
  4.05.09 (0) 6.02 (10/4) 7.05 (2.1/5) 8.01 (2.1/4)

.. index:: AI12-0237 (Ada 2022 feature)

* *AI12-0237 Getting the representation of an enumeration value (2020-01-31)*

  The GNAT-specific attributes ``Enum_Rep`` and ``Enum_Val`` have been standardized and are now also supported as Ada 2022 attributes.

  RM references: 13.04 (10) 13.04 (11/3)

.. index:: AI12-0242 (Ada 2022 feature)

* *AI12-0242 Shorthand Reduction Expressions for Objects (0000-00-00)*

  Allow reduction expressions to iterate over an an array or an iterable object without having to explicitly create a value sequence.

  This allows, for instance, writing ``A'Reduce("+", 0)`` instead of the equivalent (but more verbose) ``[for Value of A => Value]'Reduce("+", 0);``.

  RM references: 4.05.10 (0) 4.01.04 (6)

.. index:: AI12-0247 (Ada 2022 feature)

* *AI12-0247 Potentially Blocking goes too far for Detect_Blocking (0000-00-00)*

  During a protected action, a call on a subprogram that contains a potentially blocking operation is considered a bounded error (so raising P_E is optional).
  This rule imposed an unreasonable implementation burden.
  The new rule introduced by this AI allows ignoring (i.e., not detecting) the problem until execution of a potentially blocking operation is actually attempted.

  RM references: 9.05 (55/5) 9.05 (56/5) 9.05.01 (18/5) H.05 (5/2)

.. index:: AI12-0249 (Ada 2022 feature)

* *AI12-0249 User-defined numeric literals (2020-04-07)*

  Compiler support is added for three new aspects (``Integer_Literal``, ``Real_Literal``, and ``String_Literal``) as described in AI12-0249 (for ``Integer_Literal`` and ``Real_Literal``), AI12-0295 (for  ``String_Literal``), and in two follow-up AIs (AI12-0325 and AI12-0342). For pre-Ada 2022 versions of Ada, these are treated as implementation-defined
  aspects. Some implementation work remains, particularly in the interactions between these aspects and tagged types.

  RM references: 4.02 (9) 4.02.01 (0) 4.09 (3)

.. index:: AI12-0250 (Ada 2022 feature)

* *AI12-0250 Iterator Filters (2020-05-19)*

  This AI defines Ada 2022 feature of iterator filters, which can be
  applied to loop parameter specifications and iterator specifications.

  RM references: 4.03.03 (21) 4.03.03 (26) 4.03.03 (31) 4.03.05 (0) 4.05.10
  (0) 5.05 (4) 5.05 (7) 5.05 (9/4) 5.05 (9.1/4) 5.05 (10)
  5.05.02 (2/3) 5.05.02 (10/3) 5.05.02 (11/3)

.. index:: AI12-0252 (Ada 2022 feature)

* *AI12-0252 Duplicate interrupt handlers under Ravenscar (2018-07-05)*

  Ada Issue AI12-0252 requires that the runtime shall terminate with a
  Program_Error when more than one interrupt handler is attached to the same interrupt and the restriction No_Dynamic_Attachment is in effect.

  RM references: C.03.01 (13)

.. index:: AI12-0254 (Ada 2022 feature)

* *AI12-0254 Bounded_Indefinite_Holders (2025-12-15)*

  This AI defines a new package ``Ada.Containers.Indefinite_Holders`` which
  is fully implemented by GNAT.

  RM references: A.18.32 (0)

.. index:: AI12-0256 (Ada 2022 feature)

* *AI12-0256 Aspect No_Controlled_Parts (2021-01-26)*

  The compiler now supports the Ada 2022 aspect No_Controlled_Parts (see
  AI12-0256). When specified for a type, this aspect requires that the type and any of its ancestors must not have any controlled parts.

  RM references: H.04.01 (0) 13.01.01 (18.7/5)

.. index:: AI12-0258 (Ada 2022 feature)

* *AI12-0258 Containers and controlled element types (0000-00-00)*

  Most predefined containers are allowed to defer finalization of container elements until the finalization of the container. This allows implementation flexibility  but  causes problems in some cases. AI12-0258 tightens up the rules for the indefinite containers to say that finalization happens earlier - if a client needs the tighter finalization guarantees, then it can use the indefinite containers (even if the element subtype in question is definite). Other solutions involving the holder generic are also possible.

  GNAT implements these tighter element finalization requirements for instances of the indefinite container generics.

  RM references: A.18 (10/4)

.. index:: AI12-0259 (Ada 2022 feature)

* *AI12-0259 Lower bound of strings returned from Ada.Command_Line (0000-00-00)*

  Specify that the low-bound of a couple of predefined String-valued functions will always be one.

  RM references: A.15 (14) A.15 (16/3)

.. index:: AI12-0260 (Ada 2022 feature)

* *AI12-0260 Functions Is_Basic and To_Basic in Wide_Characters.Handling (2020-04-01)*

  AI12-0260 is implemented for Ada 2022, providing the new functions ``Is_Basic`` and ``To_Basic`` in package ``Ada.Wide_Characters.Handling``.

  RM references: 1.02 (8/3) A.03.05 (8/3) A.03.05 (20/3) A.03.05 (21/3)
  A.03.05 (33/3) A.03.05 (61/3)

.. index:: AI12-0261 (Ada 2022 feature)

* *AI12-0261 Conflict in "private with" rules (0000-00-00)*

  If a library unit is only visible at some point because of a "private with", there are legality rules about a name denoting that entity. The AI cleans up the wording so that it captures the intent in a corner case involving a private-child library-unit subprogram. The previous wording incorrectly caused this case to be illegal.

  RM references: 10.01.02 (12/3) 10.01.02 (13/2) 10.01.02 (14/2) 10.01.02
  (15/2) 10.01.02 (16/2)

.. index:: AI12-0262 (Ada 2022 feature)

* *AI12-0262 Map-Reduce attribute (0000-00-00)*

  The AI defines Reduction Expressions to allow the programmer to apply the
  Map-Reduce paradigm to map/transform a set of values to a new set of values,
  and then summarize/reduce the transformed values into a single result value.

  RM references: 4.01.04 (1) 4.01.04 (6) 4.01.04 (11) 4.05.10 (0)

.. index:: AI12-0263 (Ada 2022 feature)

* *AI12-0263 Update references to ISO/IEC 10646 (0000-00-00)*

  Change RM references to ISO/IEC 10646:2011 to instead refer to ISO/IEC 10646:2017. No compiler impact.

  RM references: 1.01.04 (14.2/3) 2.01 (1/3) 2.01 (3.1/3) 2.01 (4/3) 2.01
  (4.1/5) 2.01 (5/3) 2.01 (15/3) 2.01 (4.1/5) 2.01 (5/3) 2.03
  (4.1/5) 2.03 (5/3) 3.05.02 (2/3) 3.05.02 (3/3) 3.05.02 (4/3) A.01
  (36.1/3) A.01 (36.2/3) A.03.02 (32.6/5) A.03.05 (51.2/5) A.03.05
  (55/3) A.03.05 (59/3) A.04.10 (3/3) B.05 (21/5)

.. index:: AI12-0264 (Ada 2022 feature)

* *AI12-0264 Overshifting and overrotating (0000-00-00)*

  Clarify Shift and Rotate op behavior with large shift/rotate amounts.

  RM references: B.02 (9)

.. index:: AI12-0265 (Ada 2022 feature)

* *AI12-0265 Default_Initial_Condition for types (2020-11-13)*

  The aspect ``Default_Initial_Condition``, originally proposed by SPARK and
  supported in GNAT, is now also included in Ada 2022. One change from the
  original implementation is that when the aspect is specified on ancestor types of a derived type, the ancestors' check expressions also apply to the derived type.
  ``Default_Initial_Condition`` checks are also now applied in cases of default
  initialization of components, allocators, ancestor parts of extension aggregates, and box associations of aggregates.

  RM references: 7.03.03 (0) 1.01.03 (17.1/5) 11.04.02 (23.2/5) 11.04.02 (23.3/5)

.. index:: AI12-0269 (Ada 2022 feature)

* *AI12-0269 Aspect No_Return for functions reprise (2020-03-19)*

  This amendment has been implemented under the ``-gnat2022`` switch, and the
  compiler now accepts the aspect/pragma No_Return for functions and generic
  functions.

  RM references: 6.05.01 (0) 6.05.01 (1/3) 6.05.01 (3.1/3) 6.05.01 (3.4/3)
  6.05.01 (5/2) 6.05.01 (6/2) 6.05.01 (7/2) J.15.02 (2/3) J.15.02
  (3/3) J.15.02 (4/3)

.. index:: AI12-0272 (Ada 2022 feature)

* *AI12-0272 (part 1) Pre/Postconditions for formal subprograms (0000-00-00)*

  Pre and Post aspects can be specified for a generic formal subprogram. ``Default_Initial_Condition`` can be specified for a generic formal private type.

  GNAT implements this with an exception of the part related to ``Default_Initial_Condition``.

  RM references: 6.01.01 (1/5) 6.01.01 (39/5) 7.03.03 (1/5) 7.03.03 (2/5)
  7.03.03 (8/5) 7.03.04 (5/5) F.01 (1)

.. index:: AI12-0275 (Ada 2022 feature)

* *AI12-0275 Make subtype_mark optional in object renames (2020-01-28)*

  AI12-0275 allows object renamings to be declared without an explicit
  subtype_mark or access_definition. This feature can be used by compiling
  with the switch ``-gnat2022``.

  RM references: 8.05.01 (2/3) 8.05.01 (3/2)

.. index:: AI12-0277 (Ada 2022 feature)

* *AI12-0277 The meaning of "accessibility level of the body of F" (0000-00-00)*

  Clarify that the only time that an explicitly aliased formal parameter has different accessibility properties than an aliased part of a "normal" parameter is for the accessibility checking associated with a return statement.

  RM references: 3.10.02 (19.2/4)

.. index:: AI12-0278 (Ada 2022 feature)

* *AI12-0278 Implicit conversions of anonymous return types (0000-00-00)*

  If a call to a function with an anonymous-access-type result is converted to a named access type, it doesn't matter whether the conversion is implicit or explicit. the AI fixes hole where the previous rules didn't cover the implicit conversion case.

  RM references: 3.10.02 (10.3/3)

.. index:: AI12-0279 (Ada 2022 feature)

* *AI12-0279 Nonpreemptive dispatching needs more dispatching points (2020-04-17)*

  Ada 2022 defines a new aspect `Yield` that can be specified in the declaration of a noninstance subprogram (including a generic formal subprogram), a generic subprogram, or an entry, to ensure that the associated subprogram has at least one task dispatching point during each invocation.

  RM references: D.02.01 (1.5/2) D.02.01 (7/5)

.. index:: AI12-0280-2 (Ada 2022 feature)

* *AI12-0280-2 Making 'Old more flexible (2020-07-24)*

  For Ada 2022, AI12-0280-2 relaxes Ada's restrictions on 'Old attribute
  references whose attribute prefix does not statically name an entity. Previously, it was required that such an attribute reference must be unconditionally evaluated when the postcondition is evaluated; with the new rule, conditional evaluation is permitted if the relevant conditions can be evaluated upon entry to the subprogram with the same results as evaluation at the time of the postcondition's evaluation. In this case, the 'Old attribute prefix is evaluated conditionally (more specifically, the prefix is evaluated only if the result of that evaluation is going to be referenced later when the
  postcondition is evaluated).

  RM references: 6.01.01 (20/3) 6.01.01 (21/3) 6.01.01 (22/3) 6.01.01
  (22.1/4) 6.01.01 (22.2/5) 6.01.01 (23/3) 6.01.01 (24/3) 6.01.01
  (26/4) 6.01.01 (27/5) 6.01.01 (39/5)

.. index:: AI12-0282 (Ada 2022 feature)

* *AI12-0282 Atomic, Volatile, and Independent generic formal types (0000-00-00)*

  The AI specifies that the aspects ``Atomic``, ``Volatile``, ``Independent``, ``Atomic_Components``, ``Volatile_Components``, and ``Independent_Components`` are specifiable for generic formal types. The actual type must have a matching specification.

  RM references: C.06 (6.1/3) C.06 (6.3/3) C.06 (6.5/3) C.06 (6.8/3) C.06
  (12/3) C.06 (12.1/3) C.06 (21/4)

.. index:: AI12-0285 (Ada 2022 feature)

* *AI12-0285 Syntax for Stable_Properties aspects (0000-00-00)*

  The AI establishes the required named notation for a Stable_Properties aspect specification in order to avoid syntactic ambiguities.

  With the old syntax, an example like

  .. code::

     type Ugh is ...
        with Stable_Properties =\> Foo, Bar, Nonblocking, Pack;

  was problematic; ``Nonblocking`` and ``Pack`` are other aspects, while ``Foo`` and ``Bar`` are ``Stable_Properties`` functions. With the clarified syntax, the example above shall be written as:

  .. code::

      type Ugh is ...
        with Stable_Properties => (Foo, Bar), Nonblocking, Pack;

  RM references: 7.03.04 (2/5) 7.03.04 (3/5) 7.03.04 (4/5) 7.03.04 (6/5)
  7.03.04 (7/5) 7.03.04 (9/5) 7.03.04 (10/5) 7.03.04 (14/5) 13.01.01 (4/5)

.. index:: AI12-0287 (Ada 2022 feature)

* *AI12-0287 Legality Rules for null exclusions in renaming are too fierce (2020-02-17)*

  The null exclusion legality rules for generic formal object matching and object renaming now only apply to generic formal objects with mode in out.

  RM references: 8.05.01 (4.4/2) 8.05.01 (4.5/2) 8.05.01 (4.6/2) 8.05.04
  (4.2/2) 12.04 (8.3/2) 12.04 (8.4/2) 12.04 (8.5/2) 12.04 (8.2/5)
  12.06 (8.2/5)

.. index:: AI12-0289 (Ada 2022 feature)

* *AI12-0289 Implicitly null excluding anonymous access types and conformance (2020-06-09)*

  AI12-0289 is implemented for Ada 2022, allowing safer use of access parameters
  when the partial view of the designated type is untagged, but the full view is
  tagged.

  RM references: 3.10 (26)

.. index:: AI12-0290 (Ada 2022 feature)

* *AI12-0290 Restriction Pure_Barriers (2020-02-18)*

  The GNAT implementation of the Pure_Barriers restriction has
  been updated to match the Ada RM's definition as specified
  in this AI. Some constructs that were accepted by the previous
  implementation are now rejected, and vice versa. In
  particular, the use of a component of a component of a
  protected record in a barrier expression, as in "when
  Some_Component.Another_Component =>", formerly was (at least
  in some cases) not considered to be a violation of the
  Pure_Barriers restriction; that is no longer the case.

  RM references: D.07 (2) D.07 (10.10/4)

.. index:: AI12-0291 (Ada 2022 feature)

* *AI12-0291 Jorvik Profile (2020-02-19)*

  The Jorvik profile is now implemented, as defined in this AI.
  For Ada 2012 and earlier versions of Ada, Jorvik is an implementation-defined
  profile whose definition matches its Ada 2022 definition.

  RM references: D.13 (0) D.13 (1/3) D.13 (4/3) D.13 (6/4) D.13 (9/3) D.13
  (10/3) D.13 (11/4) D.13 (12/4)

.. index:: AI12-0293 (Ada 2022 feature)

* *AI12-0293 Add predefined FIFO_Streams packages (0000-00-00)*

  The AI adds ``Ada.Streams.Storage`` and its two subunits ``Bounded`` and ``Unbounded``.

  RM references: 13.13.01 (1) 13.13.01 (9) 13.13.01 (9.1/1)

.. index:: AI12-0295 (Ada 2022 feature)

* *AI12-0295 User-defined string  (2020-04-07)*

  Compiler support is added for three new aspects (``Integer_Literal``, ``Real_Literal``, and ``String_Literal``) as described in AI12-0249 (for ``Integer_Literal`` and ``Real_Literal``), AI12-0295 (for ``String_Literal``), and in two follow-up AIs (AI12-0325 and AI12-0342). For pre-Ada 2022 versions of Ada, these are treated as implementation-defined aspects. Some implementation work remains, particularly in the interactions between these aspects and tagged types.

  RM references: 4.02 (6) 4.02 (10) 4.02 (11) 3.06.03 (1) 4.02.01 (0) 4.09 (26/3)

.. index:: AI12-0301 (Ada 2022 feature)

* *AI12-0301 Predicates should be checked like constraints for types with Default_Value (2020-02-25)*

  This AI clarifies that predicate checks apply for objects that are initialized
  by default and that are of a type that has any components whose subtypes specify ``Default_Value`` or ``Default_Component_Value``.

  RM references: 3.02.04 (31/4)

.. index:: AI12-0304 (Ada 2022 feature)

* *AI12-0304 Image attributes of language-defined types (2020-07-07)*

  According to this AI, ``Put_Image`` (and therefore ``'Image``) is provided for
  the containers and for unbounded strings.

  RM references: 4.10 (0)

.. index:: AI12-0306 (Ada 2022 feature)

* *AI12-0306 Split null array aggregates from positional array aggregates (0000-00-00)*

  The AI clarifies the wording of the references RM paragraphs without introducing any language changes.

  RM references: 4.03.03 (2) 4.03.03 (3/2) 4.03.03 (9/5) 4.03.03 (26/5)
  4.03.03 (26.1/5) 4.03.03 (33/3) 4.03.03 (38) 4.03.03 (39) 4.03.03 (42)

.. index:: AI12-0307 (Ada 2022 feature)

* *AI12-0307 Resolution of aggregates (2020-08-13)*

  The proposed new syntax for aggregates in Ada 2022 uses square brackets as
  delimiters, and in particular allows ``[]`` as a notation for empty array and container aggregates. This syntax is currently available as an experimental feature under the ``-gnatX`` flag.

  RM references: 4.03 (3/5)

.. index:: AI12-0309 (Ada 2022 feature)

* *AI12-0309 Missing checks for pragma Suppress (0000-00-00)*

  The AI includes some previously overlooked run-time checks in the list of checks that are potentially suppressed via a pragma ``Suppress``. For example, AI12-0251-1 adds a check that the number of chunks in a chunk_specification is not zero or negative. Clarify that suppressing ``Program_Error_Check`` suppresses that check too.

  RM references: 11.05 (10) 11.05 (19) 11.05 (20) 11.05 (22) 11.05 (24)

.. index:: AI12-0311 (Ada 2022 feature)

* *AI12-0311 Suppressing client-side assertions for language-defined units (0000-00-00)*

  The AI defines some new assertion policies that can be given as arguments in a Suppress pragma (e.g., Calendar_Assertion_Check). GNAT recognizes and ignores those new policies, the checks are not implemented.

  RM references: 11.04.02 (23.5/5) 11.05 (23) 11.05 (26)

.. index:: AI12-0315 (Ada 2022 feature)

* *AI12-0315 Image Attributes subclause improvements (0000-00-00)*

  Clarify that a named number or similar can be the prefix of an Image attribute reference.

  RM references: 4.10 (0)

.. index:: AI12-0318 (Ada 2022 feature)

* *AI12-0318 No_IO should apply to Ada.Directories (2020-01-31)*

  The restriction No_IO now applies to and prevents the use of the
  ``Ada.Directories package``.

  RM references: H.04 (20/2) H.04 (24/3)

.. index:: AI12-0321 (Ada 2022 feature)

* *AI12-0321 Support for Arithmetic Atomic Operations and Test and Set (0000-00-00)*

  The AI adds some predefined atomic operations, e.g. package System.``Atomic_Operations.Test_And_Set``.

  RM references: C.06.03 (0) C.06.04 (0)

.. index:: AI12-0325 (Ada 2022 feature)

* *AI12-0325 Various issues with user-defined literals (2020-04-07)*

  Compiler support is added for three new aspects (``Integer_Literal``, ``Real_Literal``, and ``String_Literal``) as described in AI12-0249 (for ``Integer_Literal`` and ``Real_Literal``), AI12-0295 (for ``String_Literal``), and in two follow-up AIs (AI12-0325 and AI12-0342). For pre-Ada 2022 versions of Ada, these are treated as implementation-defined aspects. Some implementation work remains, particularly in the interactions between these aspects and tagged types.

  RM references: 4.02 (6) 4.02 (10) 4.02 (11) 4.02.01 (0)

.. index:: AI12-0329 (Ada 2022 feature)

* *AI12-0329 Naming of FIFO_Streams packages (0000-00-00)*

  The AI changes the name of predefined package ``Ada.Streams.FIFO_Streams`` to ``Ada.Streams.Storage``.

  RM references: 13.13.01 (9/5) 13.13.01 (9.1/5)

.. index:: AI12-0331 (Ada 2022 feature)

* *AI12-0331 Order of finalization of a subpool (0000-00-00)*

  Clarify that when a subpool is being finalized, objects allocated from that subpool are finalized before (not after) they cease to exist (i.e. object's storage has been reclaimed).

  RM references: 13.11.05 (5/3) 13.11.05 (6/3) 13.11.05 (7/3) 13.11.05
  (7.1/4) 13.11.05 (8/3) 13.11.05 (9/3)

.. index:: AI12-0333 (Ada 2022 feature)

* *AI12-0333 Predicate checks on out parameters (0000-00-00)*

  If a view conversion is passed as an actual parameter corresponding to an out-mode formal parameter, and if the subtype of the formal parameter has a predicate, then no predicate check associated with the conversion is performed.

  RM references: 3.02.04 (31/5) 4.06 (51/4) 6.04.01 (14)

.. index:: AI12-0335 (Ada 2022 feature)

* *AI12-0335 Dynamic accessibility check needed for some requeue targets (0000-00-00)*

  Define a new runtime accessibility check for a corner case involving requeue statements.

  RM references: 9.05.04 (7/4)

.. index:: AI12-0336 (Ada 2022 feature)

* *AI12-0336 Meaning of Time_Offset (0000-00-00)*

  The AI introduces changes to the predefined package ``Ada.Calendar.Time_Zones``.

  RM references: 9.06.01 (6/2) 9.06.01 (35/2) 9.06.01 (40/2) 9.06.01 (41/2)
  9.06.01 (42/3) 9.06.01 (90/2) 9.06.01 (91/2)

.. index:: AI12-0337 (Ada 2022 feature)

* *AI12-0337 Simple_Name("/") in Ada.Directories (0000-00-00)*

  Clarify behavior of subprograms in the predefined package ``Ada.Directories``. In particular, Simple_Name ("/") should return "/" on Unix-like systems.

  RM references: A.16 (47/2) A.16 (74/2) A.16 (82/3)

.. index:: AI12-0338 (Ada 2022 feature)

* *AI12-0338 Type invariant checking and incomplete types (0000-00-00)*

  Clarify that type invariants for type T are not checked for incomplete types whose completion is not available, even if that completion has components of type T.

  RM references: 7.03.02 (20/5)

.. index:: AI12-0339 (Ada 2022 feature)

* *AI12-0339 Empty function for Container aggregates (2020-08-06)*

  To provide uniform support for container aggregates, all standard container
  libraries have been enhanced with a function Empty, to be used when initializing an aggregate prior to inserting the specified elements in the object being constructed. All products have been updated to remove the ambiguities that may have arisen from previous uses of entities named Empty in our sources, and the expansion of container aggregates uses Empty wherever needed.

  RM references: A.18.02 (8/5) A.18.02 (12.3/5) A.18.02 (78.2/5) A.18.02
  (98.6/5) A.18.03 (6/5) A.18.03 (10.2/5) A.18.03 (50.2/5) A.18.05
  (3/5) A.18.05 (7.2/5) A.18.05 (37.3/5) A.18.05 (46/2) A.18.06
  (4/5) A.18.06 (8.2/5) A.18.06 (51.4/5) A.18.08 (3/5) A.18.08
  (8.1/5) A.18.08 (59.2/5) A.18.08 (68/2) A.18.09 (4/5) A.18.09
  (9.1/5) A.18.09 (74.2/5) A.18.10 (15.2/5) A.18.18 (8.1/5) A.18.19
  (6.1/5) A.18.20 (6/3) A.18.21 (6/3) A.18.22 (6/3) A.18.23 (6/3)
  A.18.24 (6/3) A.18.25 (8/3)

.. index:: AI12-0340 (Ada 2022 feature)

* *AI12-0340 Put_Image should use a Text_Buffer (0000-00-00)*

  Add a new predefined package Ada.Strings.Text_Buffers (along with child units) and change the definition of Put_Image attribute to refer to it.

  RM references: A.04.12 (0) 4.10 (3.1/5) 4.10 (3.2/5) 4.10 (6/5) 4.10
  (25.2/5) 4.10 (28/5) 4.10 (31/5) 4.10 (41/5) H.04 (23.2/5) H.04 (23.11/5)

.. index:: AI12-0342 (Ada 2022 feature)

* *AI12-0342 Various issues with user-defined literals (part 2) (2020-04-07)*

  Compiler support is added for three new aspects (``Integer_Literal``, ``Real_Literal``,  and ``String_Literal``) as described in AI12-0249 (for ``Integer_Literal`` and ``Real_Literal``), AI12-0295 (for ``String_Literal``), and in two follow-up AIs (AI12-0325 and AI12-0342). For pre-Ada 2022 versions of Ada, these are treated as implementation-defined aspects. Some implementation work remains, particularly in the interactions between these aspects and tagged types.

  RM references: 4.02.01 (0) 3.09.02 (1/2) 6.03.01 (22)

.. index:: AI12-0343 (Ada 2022 feature)

* *AI12-0343 Return Statement Checks (2020-04-02)*

  This binding interpretation has been implemented and the accessibility,
  predicate, and tag checks prescribed by RM 6.5 are now performed at the appropriate points, as required by this AI.

  RM references: 6.05 (5.12/5) 6.05 (8/4) 6.05 (8.1/3) 6.05 (21/3)

.. index:: AI12-0345 (Ada 2022 feature)

* *AI12-0345 Dynamic accessibility of explicitly aliased parameters (0000-00-00)*

  Further clarify (after AI12-0277) accessibility rules for explicitly aliased parameters.

  RM references: 3.10.02 (5) 3.10.02 (7/4) 3.10.02 (10.5/3) 3.10.02 (13.4/4)
  3.10.02 (19.2/5) 3.10.02 (21)

.. index:: AI12-0350 (Ada 2022 feature)

* *AI12-0350 Swap for Indefinite_Holders (2025-12-15)*

  Package ``Ada.Containers.Indefinite_Holders`` is implemented in GNAT, comprising the support for ``Swap`` as specified by this AI.

  RM references: A.18.18 (22/5) A.18.18 (67/5) A.18.18 (73/3) A.18.32 (13/5)

.. index:: AI12-0351 (Ada 2022 feature)

* *AI12-0351 Matching for actuals for formal derived types (2020-04-03)*

  This binding interpretation requires the compiler to checks
  that an actual subtype in a generic parameter association of an instantiation is statically compatible (even when the actual is unconstrained) with the ancestor of an associated nondiscriminated generic formal derived type.

  RM references: 12.05.01 (7) 12.05.01 (8)

.. index:: AI12-0352 (Ada 2022 feature)

* *AI12-0352 Early derivation and equality of untagged types (2020-07-09)*

  AI12-0352 clarifies that declaring a user-defined primitive equality operation for a record type T is illegal if it occurs after a type has been derived from T.

  RM references: 4.05.02 (9.8/4)

.. index:: AI12-0356 (Ada 2022 feature)

* *AI12-0356 Root_Storage_Pool_With_Subpools should have Preelaborable_Initialization (0000-00-00)*

  Add Preelaborable_Initialization pragmas for predefined types ``Root_Storage_Pool_With_Subpools`` and ``Root_Subpool``.

  RM references: 13.11.04 (4/3) 13.11.04 (5/3)

.. index:: AI12-0363 (Ada 2022 feature)

* *AI12-0363 Fixes for Atomic and Volatile (2020-09-08)*

  This amendment has been implemented under the ``-gnat2022`` switch and the compiler now supports the ``Full_Access_Only`` aspect, which is mostly equivalent to GNAT's ``Volatile_Full_Access``.

  RM references: 3.10.02 (26/3) 9.10 (1/5) C.06 (6.4/3) C.06 (6.10/3) C.06
  (8.1/4) C.06 (12/5) C.06 (12.1/5) C.06 (13.3/5) C.06 (19.1/5)

.. index:: AI12-0364 (Ada 2022 feature)

* *AI12-0364 Add a modular atomic arithmetic package (0000-00-00)*

  Generalize support for atomic integer operations to extend to modular types. Add new predefined generic package,
  ``System.Atomic_Operations.Modular_Arithmetic``.

  RM references: C.06.05 (0) C.06.04 (1/5) C.06.04 (2/5) C.06.04 (3/5)
  C.06.04 (9/5)

.. index:: AI12-0366 (Ada 2022 feature)

* *AI12-0366 Changes to Big_Integer and Big_Real (0000-00-00)*

  Simplify ``Big_Integer ``and ``Big_Real`` specs by eliminating explicit support for creating "invalid" values. No more
  ``Optional_Big_[Integer,Real]`` types.

  RM references: A.05.06 (0) A.05.07 (0)

.. index:: AI12-0367 (Ada 2022 feature)

* *AI12-0367 Glitches in aspect specifications (0000-00-00)*

  The AI clarifies a few wording omissions. For example, a specified Small value for a fixed point type has to be positive.

  RM references: 3.05.09 (8/2) 3.05.10 (2/1) 13.01 (9.1/5) 13.14 (10)

.. index:: AI12-0368 (Ada 2022 feature)

* *AI12-0368 Declare expressions can be static (2020-05-30)*

  AI12-0368 allows declare expressions to be static in Ada 2022.

  RM references: 4.09 (8) 4.09 (12.1/3) 4.09 (17) 6.01.01 (24.2/5) 6.01.01
  (24.3/5) 6.01.01 (24.4/5) 6.01.01 (24.5/5) C.04 (9)

.. index:: AI12-0369 (Ada 2022 feature)

* *AI12-0369 Relaxing barrier restrictions (2020-03-25)*

  The definitions of the ``Simple_Barriers`` and ``Pure_Barriers`` restrictions were modified by this AI, replacing uses of "statically denotes" with "statically names". This means that in many cases (but not all) a barrier expression that references a subcomponent of a component of the protected type while subject to either of the two restrictions is now allowed; with the previous restriction definitions, such a barrier expression would not have been legal.

  RM references: D.07 (1.3/5) D.07 (10.12/5)

.. index:: AI12-0372 (Ada 2022 feature)

* *AI12-0372 Static accessibility of "master of the call" (0000-00-00)*

  Add an extra compile-time accessibility check for explicitly aliased parameters needed to prevent dangling references.

  RM references: 3.10.02 (10.5/5) 3.10.02 (19.3/4) 6.04.01 (6.4/3)

.. index:: AI12-0373 (Ada 2022 feature)

* *AI12-0373 Bunch of fixes (0000-00-00)*

  Small clarifications to various RM entries with minor impact on compiler implementation.

  RM references: 3.01 (1) 4.02 (4) 4.02 (8/2) 4.02.01 (3/5) 4.02.01 (4/5)
  4.02.01 (5/5) 4.09 (17.3/5) 6.01.01 (41/5) 8.05.04 (4/3) 13.01.01
  (4/3) 13.01.01 (11/3) 13.14 (3/5)

.. index:: AI12-0376 (Ada 2022 feature)

* *AI12-0376 Representation changes finally allowed for untagged derived types (0000-00-00)*

  A change of representation for a derived type is allowed in some previously-illegal cases where a change of representation is required to implement a call to a derived subprogram.

  RM references: 13.01 (10/4)

.. index:: AI12-0377 (Ada 2022 feature)

* *AI12-0377 View conversions and out parameters of types with Default_Value revisited (2020-06-17)*

  This AI clarifies that an actual of an out parameter that is a view conversion
  is illegal if either the target or operand type has Default_Value specified while the other does not.

  RM references: 6.04.01 (5.1/4) 6.04.01 (5.2/4) 6.04.01 (5.3/4) 6.04.01
  (13.1/4) 6.04.01 (13.2/4) 6.04.01 (13.3/4) 6.04.01 (13.4/4) 6.04.01 (15/3)

.. index:: AI12-0381 (Ada 2022 feature)

* *AI12-0381 Tag of a delta aggregate (0000-00-00)*

  In the case of a delta aggregate of a specific tagged type, the tag of the aggregate comes from the specific type (as opposed to somehow from the base object).

  RM references: 4.03.04 (14/5)

.. index:: AI12-0382 (Ada 2022 feature)

* *AI12-0382 Loosen type-invariant overriding requirement of AI12-0042-1 (0000-00-00)*

  The AI relaxes some corner-case legality rules about type invariants that were added by AI12-0042-1.

  RM references: 7.3.2(6.1/4)

.. index:: AI12-0383 (Ada 2022 feature)

* *AI12-0383 Renaming values (2020-06-17)*

  This AI allow names that denote values rather than objects to nevertheless be
  renamed using an object renaming.

  RM references: 8.05.01 (1) 8.05.01 (4) 8.05.01 (4.1/2) 8.05.01 (6/2) 8.05.01 (8)

.. index:: AI12-0384-2 (Ada 2022 feature)

* *AI12-0384-2 Fixups for Put_Image and Text_Buffers (2021-04-29)*

  In GNAT's initial implementation of the Ada 2022 ``Put_Image`` aspect and
  attribute, buffering was performed using a GNAT-defined package,
  ``Ada.Strings.Text_Output``. Ada 2022 requires a different package, Ada.``Strings.Text_Buffers``, for this role, and that package is now provided, and the older package is eliminated.

  RM references: 4.10 (0) A.04.12 (0)

.. index:: AI12-0385 (Ada 2022 feature)

* *AI12-0385 Predefined shifts and rotates should be static (0000-00-00)*

  This AI allows Shift and Rotate operations in static expressions. GNAT implements this AI partially.

  RM references: 4.09 (20)

.. index:: AI12-0389 (Ada 2022 feature)

* *AI12-0389 Ignoring unrecognized aspects (2020-10-08)*

  Two new restrictions, ``No_Unrecognized_Aspects`` and ``No_Unrecognized_Pragmas``, are available to make the compiler emit error messages on unrecognized pragmas and aspects.

  RM references: 13.01.01 (38/3) 13.12.01 (6.3/3)

.. index:: AI12-0394 (Ada 2022 feature)

* *AI12-0394 Named Numbers and User-Defined Numeric Literals (2020-10-05)*

  Ada 2022 allows using integer named numbers with types that have an
  ``Integer_Literal`` aspect. Similarly, real named numbers may now be used with types that have a ``Real_Literal`` aspect with an overloading that takes two strings, to be used in particular  with
  ``Ada.Numerics.Big_Numbers.Big_Reals``.

  RM references: 3.03.02 (3) 4.02.01 (4/5) 4.02.01 (8/5) 4.02.01 (12/5)
  4.02.01 (13/5) 4.09 (5)

.. index:: AI12-0395 (Ada 2022 feature)

* *AI12-0395 Allow aspect_specifications on formal parameters (0000-00-00)*

  Change syntax rules to allow aspect_specifications on formal parameters, if an implementation if an implementation wants to define one. Currently, GNAT doesn't define any such aspect_specifications.

  RM references: 6.01 (15/3)

.. index:: AI12-0397 (Ada 2022 feature)

* *AI12-0397 Default_Initial_Condition applied to derived type (2020-12-09)*

  The compiler now implements the rules for resolving ``Default_Initial_Condition``
  expressions that involve references to the current instance of types with the aspect, as specified by this AI. The type of the current instance is defined to be like a formal derived type, so for a derived type that inherits the aspect, a call passing the current instance to a primitive means that the call will resolve to invoke the corresponding primitive of the descendant type. This also now permits calls to abstract primitives to occur within the aspect expression of an abstract type.

  RM references: 7.03.03 (3/5) 7.03.03 (6/5) 7.03.03 (8/5)

.. index:: AI12-0398 (Ada 2022 feature)

* *AI12-0398 Most declarations should have aspect specifications (2020-11-19)*

  It is now possible to specify aspects for discriminant specifications, extended return object declarations, and entry index specifications. This is an extension added for Ada 2022 by this AI.

  RM references: 3.07 (5/2) 6.03.01 (25) 6.05 (2.1/3) 9.05.02 (8)

.. index:: AI12-0399 (Ada 2022 feature)

* *AI12-0399 Aspect specification for Preelaborable_Initialization (0000-00-00)*

  Semantics-preserving presentation change. Replace ``Preelaborable_Initialization`` pragmas with equivalent aspect specs in the listed predefined packages. GNAT follows the guidance of this AI partially.

  RM references: 9.05 (53/5) 3.09 (6/5) 7.06 (5/2) 7.06 (7/2) 11.04.01 (2/5)
  11.04.01 (3/2) 13.11 (6/2) 13.11.04 (4/5) 13.11.04 (5/5) 13.13.01
  (3/2) A.04.02 (4/2) A.04.02 (20/2) A.04.05 (4/2) A.04.07 (4/2)
  A.04.07 (20/2) A.04.08 (4/2) A.04.08 (20/2) A.12.01 (5/4) A.18.02
  (8/5) A.18.02 (9/2) A.18.02 (79.2/5) A.18.02 (79.3/5) A.18.03
  (6/5) A.18.03 (7/2) A.18.03 (50.2/5) A.18.03 (50.3/5) A.18.05
  (3/5) A.18.05 (4/2) A.18.05 (37.3/5) A.18.05 (37.4/5) A.18.06
  (4/5) A.18.06 (5/2) A.18.06 (51.4/5) A.18.06 (51.5/5) A.18.08
  (3/5) A.18.08 (4/2) A.18.08 (58.2/5) A.18.08 (58.3/5) A.18.09
  (4/5) A.18.09 (5/2) A.18.09 (74.2/5) A.18.09 (74.3/5) A.18.10
  (8/5) A.18.10 (9/3) A.18.10 (70.2/5) A.18.10 (70.3/5) A.18.18
  (6/5) B.03.01 (5/2) C.07.01 (2/5) G.01.01 (4/2)

.. index:: AI12-0400 (Ada 2022 feature)

* *AI12-0400 Ambiguities associated with Vector Append and container aggregates (0000-00-00)*

  Change the names of subprograms in the predefined Vector containers from ``Append`` to ``Append_Vector`` and from ``Prepend`` to ``Prepend_Vector`` in order to resolve some ambiguity problems. GNAT adds the subprograms with new names but also keeps the old ones for backward compatibility.

  RM references: A.18.02 (8/5) A.18.02 (36/5) A.18.02 (37/5) A.18.02 (38/5)
  A.18.02 (44/5) A.18.02 (46/5) A.18.02 (47/5) A.18.02 (58/5)
  A.18.02 (79.2/5) A.18.02 (150/5) A.18.02 (151/5) A.18.02 (152/5)
  A.18.02 (153/5) A.18.02 (154/5) A.18.02 (155/5) A.18.02 (156/5)
  A.18.02 (168/5) A.18.02 (169/5) A.18.02 (172/5) A.18.02 (173/5)
  A.18.02 (174/5) A.18.02 (175.1/5) A.18.03 (23/5) A.18.03 (23.1/5)
  A.18.03 (58.2/5) A.18.03 (96/5) A.18.03 (97.1/5)

.. index:: AI12-0401 (Ada 2022 feature)

* *AI12-0401 Renaming of qualified expression of variable (2020-10-31)*

  Ada 2022 AI12-0401 restricts renaming of a qualified expression to cases where
  the operand is a constant, or the target subtype statically matches the nominal subtype of the operand, or is unconstrained with no predicates, to prevent setting variables to values outside their range or constraints.

  RM references: 3.03 (23.2/3) 8.05.01 (4.7/5) 8.05.01 (5/3)

.. index:: AI12-0409 (Ada 2022 feature)

* *AI12-0409 Preelaborable_Initialization and bounded containers (2021-06-23)*

  As defined by this AI, the ``Preelaborable_Initializatio`` aspect now has a
  corresponding attribute of the same name. Types declared within a generic package specification are permitted to specify the expression of a ``Prelaborable_Initialization`` aspect by including one or more references to the attribute applied to a formal private or formal derived type conjoined by ``and`` operators. This permits the full type of a private type with such an aspect expression to have components of the named formal types, and such a type will have preelaborable initialization in an instance when the
  actual types for all referenced formal types have preelaborable initialization.

  RM references: 10.02.01 (4.1/2) 10.02.01 (4.2/2) 10.02.01 (11.1/2)
  10.02.01 (11.2/2) 10.02.01 (11.6/2) 10.02.01 (11.7/2) 10.02.01
  (11.8/2) 13.01 (11/3) A.18.19 (5/5) A.18.20 (5/5) A.18.21 (5/5)
  A.18.22 (5/5) A.18.23 (5/5) A.18.24 (5/5) A.18.25 (5/5) A.18.32
  (6/5) J.15.14 (0)

.. index:: AI12-0411 (Ada 2022 feature)

* *AI12-0411 Add "bool" to Interfaces.C (0000-00-00)*

  RM references: B.03 (13) B.03 (43/2) B.03 (65.1/4)

.. index:: AI12-0412 (Ada 2022 feature)

* *AI12-0412 Abstract Pre/Post'Class on primitive of abstract type (2021-05-19)*

  In Ada 2022, by AI12-0412, it's legal to specify Pre'Class and Post'Class
  aspects on nonabstract primitive subprograms of an abstract type, but if the
  expression of such an aspect is nonstatic, then it's illegal to make a nondispatching call to such a primitive, to apply ``'Access`` to it, or to pass such a primitive as an actual subprogram for a concrete formal subprogram in a generic instantiation.

  RM references: 6.01.01 (18.2/4)

.. index:: AI12-0413 (Ada 2022 feature)

* *AI12-0413 Reemergence of "=" when defined to be abstract (0000-00-00)*

  The AI clarifies rules about operator reemergence in instances, and nondispatching calls to abstract subprograms.

  RM references: 3.09.03 (7) 4.05.02 (14.1/3) 4.05.02 (24.1/3) 12.05 (8/3)

.. index:: AI12-0423 (Ada 2022 feature)

* *AI12-0423 Aspect inheritance fixups (0000-00-00)*

  Clarify that the No_Return aspect behaves as one would expect for an inherited subprogram and that inheritance works as one would expect for a multi-part aspect whose value is specified via an aggregate (e.g., the Aggregate aspect).

  RM references: 6.05.01 (3.3/3) 13.01 (15.7/5) 13.01 (15.8/5)

.. index:: AI12-0432 (Ada 2022 feature)

* *AI12-0432 View conversions of assignments and predicate checks (2021-05-05)*

  When a predicate applies to a tagged type, a view conversion to that type
  normally requires a predicate check. However, as specified by AI12-0432, when the view conversion appears as the target of an assignment, a predicate check is not applied to the object in the conversion.

  RM references: 3.02.04 (31/5) 4.06 (51.1/5)
