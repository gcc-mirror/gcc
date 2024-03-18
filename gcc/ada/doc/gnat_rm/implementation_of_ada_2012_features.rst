.. _Implementation_of_Ada_2012_Features:

***********************************
Implementation of Ada 2012 Features
***********************************

.. index:: Ada 2012 implementation status

.. index:: -gnat12 option (gcc)

.. index:: pragma Ada_2012

.. index:: configuration pragma Ada_2012

.. index:: Ada_2012 configuration pragma

This chapter contains a complete list of Ada 2012 features that have been
implemented.
Generally, these features are only
available if the *-gnat12* (Ada 2012 features enabled) option is set,
which is the default behavior,
or if the configuration pragma ``Ada_2012`` is used.

However, new pragmas, attributes, and restrictions are
unconditionally available, since the Ada 95 standard allows the addition of
new pragmas, attributes, and restrictions (there are exceptions, which are
documented in the individual descriptions), and also certain packages
were made available in earlier versions of Ada.

An ISO date (YYYY-MM-DD) appears in parentheses on the description line.
This date shows the implementation date of the feature. Any wavefront
subsequent to this date will contain the indicated feature, as will any
subsequent releases. A date of 0000-00-00 means that GNAT has always
implemented the feature, or implemented it as soon as it appeared as a
binding interpretation.

Each feature corresponds to an Ada Issue ('AI') approved by the Ada
standardization group (ISO/IEC JTC1/SC22/WG9) for inclusion in Ada 2012.
The features are ordered based on the relevant sections of the Ada
Reference Manual ("RM").  When a given AI relates to multiple points
in the RM, the earliest is used.

A complete description of the AIs may be found in
http://www.ada-auth.org/ai05-summary.html.

.. index:: AI-0002 (Ada 2012 feature)

* *AI-0002 Export C with unconstrained arrays (0000-00-00)*

  The compiler is not required to support exporting an Ada subprogram with
  convention C if there are parameters or a return type of an unconstrained
  array type (such as ``String``). GNAT allows such declarations but
  generates warnings. It is possible, but complicated, to write the
  corresponding C code and certainly such code would be specific to GNAT and
  non-portable.

  RM References:  B.01 (17)   B.03 (62)   B.03 (71.1/2)

.. index:: AI-0003 (Ada 2012 feature)

* *AI-0003 Qualified expressions as names (2010-07-11)*

  In Ada 2012, a qualified expression is considered to be syntactically a name,
  meaning that constructs such as ``A'(F(X)).B`` are now legal. This is
  useful in disambiguating some cases of overloading.

  RM References:  3.03 (11)   3.03 (21)   4.01 (2)   4.04 (7)   4.07 (3)
  5.04 (7)

.. index:: AI-0007 (Ada 2012 feature)

* *AI-0007 Stream read and private scalar types (0000-00-00)*

  The RM as written appeared to limit the possibilities of declaring read
  attribute procedures for private scalar types. This limitation was not
  intended, and has never been enforced by GNAT.

  RM References:  13.13.02 (50/2)   13.13.02 (51/2)

.. index:: AI-0008 (Ada 2012 feature)

* *AI-0008 General access to constrained objects (0000-00-00)*

  The wording in the RM implied that if you have a general access to a
  constrained object, it could be used to modify the discriminants. This was
  obviously not intended. ``Constraint_Error`` should be raised, and GNAT
  has always done so in this situation.

  RM References:  3.03 (23)   3.10.02 (26/2)   4.01 (9)   6.04.01 (17)   8.05.01 (5/2)

.. index:: AI-0009 (Ada 2012 feature)

* *AI-0009 Pragma Independent[_Components] (2010-07-23)*

  This AI introduces the new pragmas ``Independent`` and
  ``Independent_Components``,
  which control guaranteeing independence of access to objects and components.
  The AI also requires independence not unaffected by confirming rep clauses.

  RM References:  9.10 (1)   13.01 (15/1)   13.02 (9)   13.03 (13)   C.06 (2)
  C.06 (4)   C.06 (6)   C.06 (9)   C.06 (13)   C.06 (14)

.. index:: AI-0012 (Ada 2012 feature)

* *AI-0012 Pack/Component_Size for aliased/atomic (2010-07-15)*

  It is now illegal to give an inappropriate component size or a pragma
  ``Pack`` that attempts to change the component size in the case of atomic
  or aliased components. Previously GNAT ignored such an attempt with a
  warning.

  RM References:  13.02 (6.1/2)   13.02 (7)   C.06 (10)   C.06 (11)   C.06 (21)

.. index:: AI-0015 (Ada 2012 feature)

* *AI-0015 Constant return objects (0000-00-00)*

  The return object declared in an *extended_return_statement* may be
  declared constant. This was always intended, and GNAT has always allowed it.

  RM References:  6.05 (2.1/2)   3.03 (10/2)   3.03 (21)   6.05 (5/2)
  6.05 (5.7/2)

.. index:: AI-0017 (Ada 2012 feature)

* *AI-0017 Freezing and incomplete types (0000-00-00)*

  So-called 'Taft-amendment types' (i.e., types that are completed in package
  bodies) are not frozen by the occurrence of bodies in the
  enclosing declarative part. GNAT always implemented this properly.

  RM References:  13.14 (3/1)

.. index:: AI-0019 (Ada 2012 feature)

* *AI-0019 Freezing of primitives for tagged types (0000-00-00)*

  The RM suggests that primitive subprograms of a specific tagged type are
  frozen when the tagged type is frozen. This would be an incompatible change
  and is not intended. GNAT has never attempted this kind of freezing and its
  behavior is consistent with the recommendation of this AI.

  RM References:  13.14 (2)   13.14 (3/1)   13.14 (8.1/1)   13.14 (10)   13.14 (14)   13.14 (15.1/2)

.. index:: AI-0026 (Ada 2012 feature)

* *AI-0026 Missing rules for Unchecked_Union (2010-07-07)*

  Record representation clauses concerning Unchecked_Union types cannot mention
  the discriminant of the type. The type of a component declared in the variant
  part of an Unchecked_Union cannot be controlled, have controlled components,
  nor have protected or task parts. If an Unchecked_Union type is declared
  within the body of a generic unit or its descendants, then the type of a
  component declared in the variant part cannot be a formal private type or a
  formal private extension declared within the same generic unit.

  RM References:  7.06 (9.4/2)   B.03.03 (9/2)   B.03.03 (10/2)

.. index:: AI-0030 (Ada 2012 feature)

* *AI-0030 Requeue on synchronized interfaces (2010-07-19)*

  Requeue is permitted to a protected, synchronized or task interface primitive
  providing it is known that the overriding operation is an entry. Otherwise
  the requeue statement has the same effect as a procedure call. Use of pragma
  ``Implemented`` provides a way to impose a static requirement on the
  overriding operation by adhering to one of the implementation kinds: entry,
  protected procedure or any of the above.

  RM References:  9.05 (9)   9.05.04 (2)   9.05.04 (3)   9.05.04 (5)
  9.05.04 (6)   9.05.04 (7)   9.05.04 (12)

.. index:: AI-0031 (Ada 2012 feature)

* *AI-0031 Add From parameter to Find_Token (2010-07-25)*

  A new version of ``Find_Token`` is added to all relevant string packages,
  with an extra parameter ``From``. Instead of starting at the first
  character of the string, the search for a matching Token starts at the
  character indexed by the value of ``From``.
  These procedures are available in all versions of Ada
  but if used in versions earlier than Ada 2012 they will generate a warning
  that an Ada 2012 subprogram is being used.

  RM References:  A.04.03 (16)   A.04.03 (67)   A.04.03 (68/1)   A.04.04 (51)
  A.04.05 (46)

.. index:: AI-0032 (Ada 2012 feature)

* *AI-0032 Extended return for class-wide functions (0000-00-00)*

  If a function returns a class-wide type, the object of an extended return
  statement can be declared with a specific type that is covered by the class-
  wide type. This has been implemented in GNAT since the introduction of
  extended returns. Note AI-0103 complements this AI by imposing matching
  rules for constrained return types.

  RM References:  6.05 (5.2/2)   6.05 (5.3/2)   6.05 (5.6/2)   6.05 (5.8/2)
  6.05 (8/2)

.. index:: AI-0033 (Ada 2012 feature)

* *AI-0033 Attach/Interrupt_Handler in generic (2010-07-24)*

  Neither of these two pragmas may appear within a generic template, because
  the generic might be instantiated at other than the library level.

  RM References:  13.11.02 (16)   C.03.01 (7/2)   C.03.01 (8/2)

.. index:: AI-0034 (Ada 2012 feature)

* *AI-0034 Categorization of limited views (0000-00-00)*

  The RM makes certain limited with clauses illegal because of categorization
  considerations, when the corresponding normal with would be legal. This is
  not intended, and GNAT has always implemented the recommended behavior.

  RM References:  10.02.01 (11/1)   10.02.01 (17/2)

.. index:: AI-0035 (Ada 2012 feature)

* *AI-0035 Inconsistencies with Pure units (0000-00-00)*

  This AI remedies some inconsistencies in the legality rules for Pure units.
  Derived access types are legal in a pure unit (on the assumption that the
  rule for a zero storage pool size has been enforced on the ancestor type).
  The rules are enforced in generic instances and in subunits. GNAT has always
  implemented the recommended behavior.

  RM References:  10.02.01 (15.1/2)   10.02.01 (15.4/2)   10.02.01 (15.5/2)   10.02.01 (17/2)

.. index:: AI-0037 (Ada 2012 feature)

* *AI-0037 Out-of-range box associations in aggregate (0000-00-00)*

  This AI confirms that an association of the form ``Indx => <>`` in an
  array aggregate must raise ``Constraint_Error`` if ``Indx``
  is out of range. The RM specified a range check on other associations, but
  not when the value of the association was defaulted. GNAT has always inserted
  a constraint check on the index value.

  RM References:  4.03.03 (29)

.. index:: AI-0038 (Ada 2012 feature)

* *AI-0038 Minor errors in Text_IO (0000-00-00)*

  These are minor errors in the description on three points. The intent on
  all these points has always been clear, and GNAT has always implemented the
  correct intended semantics.

  RM References:  A.10.05 (37)   A.10.07 (8/1)   A.10.07 (10)   A.10.07 (12)   A.10.08 (10)   A.10.08 (24)

.. index:: AI-0039 (Ada 2012 feature)

* *AI-0039 Stream attributes cannot be dynamic (0000-00-00)*

  The RM permitted the use of dynamic expressions (such as ``ptr.all``)
  for stream attributes, but these were never useful and are now illegal. GNAT
  has always regarded such expressions as illegal.

  RM References:  13.03 (4)   13.03 (6)   13.13.02 (38/2)

.. index:: AI-0040 (Ada 2012 feature)

* *AI-0040 Limited with clauses on descendant (0000-00-00)*

  This AI confirms that a limited with clause in a child unit cannot name
  an ancestor of the unit. This has always been checked in GNAT.

  RM References:  10.01.02 (20/2)

.. index:: AI-0042 (Ada 2012 feature)

* *AI-0042 Overriding versus implemented-by (0000-00-00)*

  This AI fixes a wording gap in the RM. An operation of a synchronized
  interface can be implemented by a protected or task entry, but the abstract
  operation is not being overridden in the usual sense, and it must be stated
  separately that this implementation is legal. This has always been the case
  in GNAT.

  RM References:  9.01 (9.2/2)   9.04 (11.1/2)

.. index:: AI-0043 (Ada 2012 feature)

* *AI-0043 Rules about raising exceptions (0000-00-00)*

  This AI covers various omissions in the RM regarding the raising of
  exceptions. GNAT has always implemented the intended semantics.

  RM References:  11.04.01 (10.1/2)   11 (2)

.. index:: AI-0044 (Ada 2012 feature)

* *AI-0044 Restrictions on container instantiations (0000-00-00)*

  This AI places restrictions on allowed instantiations of generic containers.
  These restrictions are not checked by the compiler, so there is nothing to
  change in the implementation. This affects only the RM documentation.

  RM References:  A.18 (4/2)   A.18.02 (231/2)   A.18.03 (145/2)   A.18.06 (56/2)   A.18.08 (66/2)   A.18.09 (79/2)   A.18.26 (5/2)   A.18.26 (9/2)

.. index:: AI-0046 (Ada 2012 feature)

* *AI-0046 Null exclusion match for full conformance (2010-07-17)*

  For full conformance, in the case of access parameters, the null exclusion
  must match (either both or neither must have ``not null``).

  RM References:  6.03.02 (18)

.. index:: AI-0050 (Ada 2012 feature)

* *AI-0050 Raising Constraint_Error early for function call (0000-00-00)*

  The implementation permissions for raising ``Constraint_Error`` early on a function call
  when it was clear an exception would be raised were over-permissive and allowed
  mishandling of discriminants in some cases. GNAT did
  not take advantage of these incorrect permissions in any case.

  RM References:  6.05 (24/2)

.. index:: AI-0056 (Ada 2012 feature)

* *AI-0056 Index on null string returns zero (0000-00-00)*

  The wording in the Ada 2005 RM implied an incompatible handling of the
  ``Index`` functions, resulting in raising an exception instead of
  returning zero in some situations.
  This was not intended and has been corrected.
  GNAT always returned zero, and is thus consistent with this AI.

  RM References:  A.04.03 (56.2/2)   A.04.03 (58.5/2)

.. index:: AI-0058 (Ada 2012 feature)

* *AI-0058 Abnormal completion of an extended return (0000-00-00)*

  The RM had some incorrect wording implying wrong treatment of abnormal
  completion in an extended return. GNAT has always implemented the intended
  correct semantics as described by this AI.

  RM References:  6.05 (22/2)

.. index:: AI-0060 (Ada 2012 feature)

* *AI-0060 Extended definition of remote access types (0000-00-00)*

  This AI extends the definition of remote access types to include access
  to limited, synchronized, protected or task class-wide interface types.
  GNAT already implemented this extension.

  RM References:  A (4)   E.02.02 (9/1)   E.02.02 (9.2/1)   E.02.02 (14/2)   E.02.02 (18)

.. index:: AI-0062 (Ada 2012 feature)

* *AI-0062 Null exclusions and deferred constants (0000-00-00)*

  A full constant may have a null exclusion even if its associated deferred
  constant does not. GNAT has always allowed this.

  RM References:  7.04 (6/2)   7.04 (7.1/2)

.. index:: AI-0064 (Ada 2012 feature)

* *AI-0064 Redundant finalization rule (0000-00-00)*

  This is an editorial change only. The intended behavior is already checked
  by an existing ACATS test, which GNAT has always executed correctly.

  RM References:  7.06.01 (17.1/1)

.. index:: AI-0065 (Ada 2012 feature)

* *AI-0065 Remote access types and external streaming (0000-00-00)*

  This AI clarifies the fact that all remote access types support external
  streaming. This fixes an obvious oversight in the definition of the
  language, and GNAT always implemented the intended correct rules.

  RM References:  13.13.02 (52/2)

.. index:: AI-0070 (Ada 2012 feature)

* *AI-0070 Elaboration of interface types (0000-00-00)*

  This is an editorial change only, there are no testable consequences short of
  checking for the absence of generated code for an interface declaration.

  RM References:  3.09.04 (18/2)

.. index:: AI-0072 (Ada 2012 feature)

* *AI-0072 Task signalling using 'Terminated (0000-00-00)*

  This AI clarifies that task signalling for reading ``'Terminated`` only
  occurs if the result is True. GNAT semantics has always been consistent with
  this notion of task signalling.

  RM References:  9.10 (6.1/1)

.. index:: AI-0073 (Ada 2012 feature)

* *AI-0073 Functions returning abstract types (2010-07-10)*

  This AI covers a number of issues regarding returning abstract types. In
  particular generic functions cannot have abstract result types or access
  result types designated an abstract type. There are some other cases which
  are detailed in the AI. Note that this binding interpretation has not been
  retrofitted to operate before Ada 2012 mode, since it caused a significant
  number of regressions.

  RM References:  3.09.03 (8)   3.09.03 (10)   6.05 (8/2)

.. index:: AI-0076 (Ada 2012 feature)

* *AI-0076 function with controlling result (0000-00-00)*

  This is an editorial change only. The RM defines calls with controlling
  results, but uses the term 'function with controlling result' without an
  explicit definition.

  RM References:  3.09.02 (2/2)

.. index:: AI-0077 (Ada 2012 feature)

* *AI-0077 Limited withs and scope of declarations (0000-00-00)*

  This AI clarifies that a declaration does not include a context clause,
  and confirms that it is illegal to have a context in which both a limited
  and a nonlimited view of a package are accessible. Such double visibility
  was always rejected by GNAT.

  RM References:  10.01.02 (12/2)   10.01.02 (21/2)   10.01.02 (22/2)

.. index:: AI-0078 (Ada 2012 feature)

* *AI-0078 Relax Unchecked_Conversion alignment rules (0000-00-00)*

  In Ada 2012, compilers are required to support unchecked conversion where the
  target alignment is a multiple of the source alignment. GNAT always supported
  this case (and indeed all cases of differing alignments, doing copies where
  required if the alignment was reduced).

  RM References:  13.09 (7)

.. index:: AI-0079 (Ada 2012 feature)

* *AI-0079 Allow other_format characters in source (2010-07-10)*

  Wide characters in the unicode category *other_format* are now allowed in
  source programs between tokens, but not within a token such as an identifier.

  RM References:  2.01 (4/2)   2.02 (7)

.. index:: AI-0080 (Ada 2012 feature)

* *AI-0080 'View of' not needed if clear from context (0000-00-00)*

  This is an editorial change only, described as non-testable in the AI.

  RM References:  3.01 (7)

.. index:: AI-0087 (Ada 2012 feature)

* *AI-0087 Actual for formal nonlimited derived type (2010-07-15)*

  The actual for a formal nonlimited derived type cannot be limited. In
  particular, a formal derived type that extends a limited interface but which
  is not explicitly limited cannot be instantiated with a limited type.

  RM References:  7.05 (5/2)   12.05.01 (5.1/2)

.. index:: AI-0088 (Ada 2012 feature)

* *AI-0088 The value of exponentiation (0000-00-00)*

  This AI clarifies the equivalence rule given for the dynamic semantics of
  exponentiation: the value of the operation can be obtained by repeated
  multiplication, but the operation can be implemented otherwise (for example
  using the familiar divide-by-two-and-square algorithm, even if this is less
  accurate), and does not imply repeated reads of a volatile base.

  RM References:  4.05.06 (11)

.. index:: AI-0091 (Ada 2012 feature)

* *AI-0091 Do not allow other_format in identifiers (0000-00-00)*

  Wide characters in the unicode category *other_format* are not permitted
  within  an identifier, since this can be a security problem. The error
  message for this case has been improved to be more specific, but GNAT has
  never allowed such characters to appear in identifiers.

  RM References:  2.03 (3.1/2)   2.03 (4/2)   2.03 (5/2)   2.03 (5.1/2)   2.03 (5.2/2)   2.03 (5.3/2)   2.09 (2/2)

.. index:: AI-0093 (Ada 2012 feature)

* *AI-0093 Additional rules use immutably limited (0000-00-00)*

  This is an editorial change only, to make more widespread use of the Ada 2012
  'immutably limited'.

  RM References:  3.03 (23.4/3)

.. index:: AI-0095 (Ada 2012 feature)

* *AI-0095 Address of intrinsic subprograms (0000-00-00)*

  The prefix of ``'Address`` cannot statically denote a subprogram with
  convention ``Intrinsic``. The use of the ``Address`` attribute raises
  ``Program_Error`` if the prefix denotes a subprogram with convention
  ``Intrinsic``.

  RM References:  13.03 (11/1)

.. index:: AI-0096 (Ada 2012 feature)

* *AI-0096 Deriving from formal private types (2010-07-20)*

  In general it is illegal for a type derived from a formal limited type to be
  nonlimited.  This AI makes an exception to this rule: derivation is legal
  if it appears in the private part of the generic, and the formal type is not
  tagged. If the type is tagged, the legality check must be applied to the
  private part of the package.

  RM References:  3.04 (5.1/2)   6.02 (7)

.. index:: AI-0097 (Ada 2012 feature)

* *AI-0097 Treatment of abstract null extension (2010-07-19)*

  The RM as written implied that in some cases it was possible to create an
  object of an abstract type, by having an abstract extension inherit a non-
  abstract constructor from its parent type. This mistake has been corrected
  in GNAT and in the RM, and this construct is now illegal.

  RM References:  3.09.03 (4/2)

.. index:: AI-0098 (Ada 2012 feature)

* *AI-0098 Anonymous subprogram access restrictions (0000-00-00)*

  An unintentional omission in the RM implied some inconsistent restrictions on
  the use of anonymous access to subprogram values. These restrictions were not
  intentional, and have never been enforced by GNAT.

  RM References:  3.10.01 (6)   3.10.01 (9.2/2)

.. index:: AI-0099 (Ada 2012 feature)

* *AI-0099 Tag determines whether finalization needed (0000-00-00)*

  This AI clarifies that 'needs finalization' is part of dynamic semantics,
  and therefore depends on the run-time characteristics of an object (i.e. its
  tag) and not on its nominal type. As the AI indicates: "we do not expect
  this to affect any implementation".

  RM References:  7.06.01 (6)   7.06.01 (7)   7.06.01 (8)   7.06.01 (9/2)

.. index:: AI-0100 (Ada 2012 feature)

* *AI-0100 Placement of pragmas  (2010-07-01)*

  This AI is an earlier version of AI-163. It simplifies the rules
  for legal placement of pragmas. In the case of lists that allow pragmas, if
  the list may have no elements, then the list may consist solely of pragmas.

  RM References:  2.08 (7)

.. index:: AI-0102 (Ada 2012 feature)

* *AI-0102 Some implicit conversions are illegal (0000-00-00)*

  It is illegal to assign an anonymous access constant to an anonymous access
  variable. The RM did not have a clear rule to prevent this, but GNAT has
  always generated an error for this usage.

  RM References:  3.07 (16)   3.07.01 (9)   6.04.01 (6)   8.06 (27/2)

.. index:: AI-0103 (Ada 2012 feature)

* *AI-0103 Static matching for extended return (2010-07-23)*

  If the return subtype of a function is an elementary type or a constrained
  type, the subtype indication in an extended return statement must match
  statically this return subtype.

  RM References:  6.05 (5.2/2)

.. index:: AI-0104 (Ada 2012 feature)

* *AI-0104 Null exclusion and uninitialized allocator (2010-07-15)*

  The assignment ``Ptr := new not null Some_Ptr;`` will raise
  ``Constraint_Error`` because the default value of the allocated object is
  **null**. This useless construct is illegal in Ada 2012.

  RM References:  4.08 (2)

.. index:: AI-0106 (Ada 2012 feature)

* *AI-0106 No representation pragmas on generic formals (0000-00-00)*

  The RM appeared to allow representation pragmas on generic formal parameters,
  but this was not intended, and GNAT has never permitted this usage.

  RM References:  13.01 (9.1/1)

.. index:: AI-0108 (Ada 2012 feature)

* *AI-0108 Limited incomplete view and discriminants (0000-00-00)*

  This AI confirms that an incomplete type from a limited view does not have
  discriminants. This has always been the case in GNAT.

  RM References:  10.01.01 (12.3/2)

.. index:: AI-0109 (Ada 2012 feature)

* *AI-0109 Redundant check in S'Class'Input (0000-00-00)*

  This AI is an editorial change only. It removes the need for a tag check
  that can never fail.

  RM References:  13.13.02 (34/2)

.. index:: AI-0112 (Ada 2012 feature)

* *AI-0112 Detection of duplicate pragmas (2010-07-24)*

  This AI concerns giving names to various representation aspects, but the
  practical effect is simply to make the use of duplicate
  ``Atomic[_Components]``,
  ``Volatile[_Components]``, and
  ``Independent[_Components]`` pragmas illegal, and GNAT
  now performs this required check.

  RM References:  13.01 (8)

.. index:: AI-0114 (Ada 2012 feature)

* *AI-0114 Classification of letters (0000-00-00)*

  The code points 170 (``FEMININE ORDINAL INDICATOR``),
  181 (``MICRO SIGN``), and
  186 (``MASCULINE ORDINAL INDICATOR``) are technically considered
  lower case letters by Unicode.
  However, they are not allowed in identifiers, and they
  return ``False`` to ``Ada.Characters.Handling.Is_Letter/Is_Lower``.
  This behavior is consistent with that defined in Ada 95.

  RM References:  A.03.02 (59)   A.04.06 (7)

.. index:: AI-0116 (Ada 2012 feature)

* *AI-0116 Alignment of class-wide objects (0000-00-00)*

  This AI requires that the alignment of a class-wide object be no greater
  than the alignment of any type in the class. GNAT has always followed this
  recommendation.

  RM References:  13.03 (29)   13.11 (16)

.. index:: AI-0118 (Ada 2012 feature)

* *AI-0118 The association of parameter associations (0000-00-00)*

  This AI clarifies the rules for named associations in subprogram calls and
  generic instantiations. The rules have been in place since Ada 83.

  RM References:  6.04.01 (2)   12.03 (9)

.. index:: AI-0120 (Ada 2012 feature)

* *AI-0120 Constant instance of protected object (0000-00-00)*

  This is an RM editorial change only. The section that lists objects that are
  constant failed to include the current instance of a protected object
  within a protected function. This has always been treated as a constant
  in GNAT.

  RM References:  3.03 (21)

.. index:: AI-0122 (Ada 2012 feature)

* *AI-0122 Private with and children of generics (0000-00-00)*

  This AI clarifies the visibility of private children of generic units within
  instantiations of a parent. GNAT has always handled this correctly.

  RM References:  10.01.02 (12/2)

.. index:: AI-0123 (Ada 2012 feature)

* *AI-0123 Composability of equality (2010-04-13)*

  Equality of untagged record composes, so that the predefined equality for a
  composite type that includes a component of some untagged record type
  ``R`` uses the equality operation of ``R`` (which may be user-defined
  or predefined). This makes the behavior of untagged records identical to that
  of tagged types in this respect.

  This change is an incompatibility with previous versions of Ada, but it
  corrects a non-uniformity that was often a source of confusion. Analysis of
  a large number of industrial programs indicates that in those rare cases
  where a composite type had an untagged record component with a user-defined
  equality, either there was no use of the composite equality, or else the code
  expected the same composability as for tagged types, and thus had a bug that
  would be fixed by this change.

  RM References:  4.05.02 (9.7/2)   4.05.02 (14)   4.05.02 (15)   4.05.02 (24)
  8.05.04 (8)

.. index:: AI-0125 (Ada 2012 feature)

* *AI-0125 Nonoverridable operations of an ancestor (2010-09-28)*

  In Ada 2012, the declaration of a primitive operation of a type extension
  or private extension can also override an inherited primitive that is not
  visible at the point of this declaration.

  RM References:  7.03.01 (6)   8.03 (23)   8.03.01 (5/2)   8.03.01 (6/2)

.. index:: AI-0126 (Ada 2012 feature)

* *AI-0126 Dispatching with no declared operation (0000-00-00)*

  This AI clarifies dispatching rules, and simply confirms that dispatching
  executes the operation of the parent type when there is no explicitly or
  implicitly declared operation for the descendant type. This has always been
  the case in all versions of GNAT.

  RM References:  3.09.02 (20/2)   3.09.02 (20.1/2)   3.09.02 (20.2/2)

.. index:: AI-0127 (Ada 2012 feature)

* *AI-0127 Adding Locale Capabilities (2010-09-29)*

  This package provides an interface for identifying the current locale.

  RM References:  A.19    A.19.01    A.19.02    A.19.03    A.19.05    A.19.06
  A.19.07    A.19.08    A.19.09    A.19.10    A.19.11    A.19.12    A.19.13

.. index:: AI-0128 (Ada 2012 feature)

* *AI-0128 Inequality is a primitive operation (0000-00-00)*

  If an equality operator ("=") is declared for a type, then the implicitly
  declared inequality operator ("/=") is a primitive operation of the type.
  This is the only reasonable interpretation, and is the one always implemented
  by GNAT, but the RM was not entirely clear in making this point.

  RM References:  3.02.03 (6)   6.06 (6)

.. index:: AI-0129 (Ada 2012 feature)

* *AI-0129 Limited views and incomplete types (0000-00-00)*

  This AI clarifies the description of limited views: a limited view of a
  package includes only one view of a type that has an incomplete declaration
  and a full declaration (there is no possible ambiguity in a client package).
  This AI also fixes an omission: a nested package in the private part has no
  limited view. GNAT always implemented this correctly.

  RM References:  10.01.01 (12.2/2)   10.01.01 (12.3/2)

.. index:: AI-0132 (Ada 2012 feature)

* *AI-0132 Placement of library unit pragmas (0000-00-00)*

  This AI fills a gap in the description of library unit pragmas. The pragma
  clearly must apply to a library unit, even if it does not carry the name
  of the enclosing unit. GNAT has always enforced the required check.

  RM References:  10.01.05 (7)

.. index:: AI-0134 (Ada 2012 feature)

* *AI-0134 Profiles must match for full conformance (0000-00-00)*

  For full conformance, the profiles of anonymous-access-to-subprogram
  parameters must match. GNAT has always enforced this rule.

  RM References:  6.03.01 (18)

.. index:: AI-0137 (Ada 2012 feature)

* *AI-0137 String encoding package (2010-03-25)*

  The packages ``Ada.Strings.UTF_Encoding``, together with its child
  packages, ``Conversions``, ``Strings``, ``Wide_Strings``,
  and ``Wide_Wide_Strings`` have been
  implemented. These packages (whose documentation can be found in the spec
  files :file:`a-stuten.ads`, :file:`a-suenco.ads`, :file:`a-suenst.ads`,
  :file:`a-suewst.ads`, :file:`a-suezst.ads`) allow encoding and decoding of
  ``String``, ``Wide_String``, and ``Wide_Wide_String``
  values using UTF coding schemes (including UTF-8, UTF-16LE, UTF-16BE, and
  UTF-16), as well as conversions between the different UTF encodings. With
  the exception of ``Wide_Wide_Strings``, these packages are available in
  Ada 95 and Ada 2005 mode as well as Ada 2012 mode.
  The ``Wide_Wide_Strings`` package
  is available in Ada 2005 mode as well as Ada 2012 mode (but not in Ada 95
  mode since it uses ``Wide_Wide_Character``).

  RM References:  A.04.11

.. index:: AI-0139-2 (Ada 2012 feature)

* *AI-0139-2 Syntactic sugar for iterators (2010-09-29)*

  The new syntax for iterating over arrays and containers is now implemented.
  Iteration over containers is for now limited to read-only iterators. Only
  default iterators are supported, with the syntax: ``for Elem of C``.

  RM References:  5.05

.. index:: AI-0146 (Ada 2012 feature)

* *AI-0146 Type invariants (2009-09-21)*

  Type invariants may be specified for private types using the aspect notation.
  Aspect ``Type_Invariant`` may be specified for any private type,
  ``Type_Invariant'Class`` can
  only be specified for tagged types, and is inherited by any descendent of the
  tagged types. The invariant is a boolean expression that is tested for being
  true in the following situations: conversions to the private type, object
  declarations for the private type that are default initialized, and
  [**in**] **out**
  parameters and returned result on return from any primitive operation for
  the type that is visible to a client.
  GNAT defines the synonyms ``Invariant`` for ``Type_Invariant`` and
  ``Invariant'Class`` for ``Type_Invariant'Class``.

  RM References:  13.03.03 (00)

.. index:: AI-0147 (Ada 2012 feature)

* *AI-0147 Conditional expressions (2009-03-29)*

  Conditional expressions are permitted. The form of such an expression is:

  ::

        (if expr then expr {elsif expr then expr} [else expr])

  The parentheses can be omitted in contexts where parentheses are present
  anyway, such as subprogram arguments and pragma arguments. If the **else**
  clause is omitted, **else** *True* is assumed;
  thus ``(if A then B)`` is a way to conveniently represent
  *(A implies B)* in standard logic.

  RM References:  4.03.03 (15)   4.04 (1)   4.04 (7)   4.05.07 (0)   4.07 (2)
  4.07 (3)   4.09 (12)   4.09 (33)   5.03 (3)   5.03 (4)   7.05 (2.1/2)

.. index:: AI-0152 (Ada 2012 feature)

* *AI-0152 Restriction No_Anonymous_Allocators (2010-09-08)*

  Restriction ``No_Anonymous_Allocators`` prevents the use of allocators
  where the type of the returned value is an anonymous access type.

  RM References:  H.04 (8/1)

.. index:: AI-0157 (Ada 2012 feature)

* *AI-0157 Allocation/Deallocation from empty pool (2010-07-11)*

  Allocation and Deallocation from an empty storage pool (i.e. allocation or
  deallocation of a pointer for which a static storage size clause of zero
  has been given) is now illegal and is detected as such. GNAT
  previously gave a warning but not an error.

  RM References:  4.08 (5.3/2)   13.11.02 (4)   13.11.02 (17)

.. index:: AI-0158 (Ada 2012 feature)

* *AI-0158 Generalizing membership tests (2010-09-16)*

  This AI extends the syntax of membership tests to simplify complex conditions
  that can be expressed as membership in a subset of values of any type. It
  introduces syntax for a list of expressions that may be used in loop contexts
  as well.

  RM References:  3.08.01 (5)   4.04 (3)   4.05.02 (3)   4.05.02 (5)   4.05.02 (27)

.. index:: AI-0161 (Ada 2012 feature)

* *AI-0161 Restriction No_Default_Stream_Attributes (2010-09-11)*

  A new restriction ``No_Default_Stream_Attributes`` prevents the use of any
  of the default stream attributes for elementary types. If this restriction is
  in force, then it is necessary to provide explicit subprograms for any
  stream attributes used.

  RM References:  13.12.01 (4/2)   13.13.02 (40/2)   13.13.02 (52/2)

.. index:: AI-0162 (Ada 2012 feature)

* *AI-0162 Incomplete type completed by partial view (2010-09-15)*

  Incomplete types are made more useful by allowing them to be completed by
  private types and private extensions.

  RM References:  3.10.01 (2.5/2)   3.10.01 (2.6/2)   3.10.01 (3)   3.10.01 (4/2)

.. index:: AI-0163 (Ada 2012 feature)

* *AI-0163 Pragmas in place of null (2010-07-01)*

  A statement sequence may be composed entirely of pragmas. It is no longer
  necessary to add a dummy ``null`` statement to make the sequence legal.

  RM References:  2.08 (7)   2.08 (16)

.. index:: AI-0171 (Ada 2012 feature)

* *AI-0171 Pragma CPU and Ravenscar Profile (2010-09-24)*

  A new package ``System.Multiprocessors`` is added, together with the
  definition of pragma ``CPU`` for controlling task affinity. A new no
  dependence restriction, on ``System.Multiprocessors.Dispatching_Domains``,
  is added to the Ravenscar profile.

  RM References:  D.13.01 (4/2)   D.16

.. index:: AI-0173 (Ada 2012 feature)

* *AI-0173 Testing if tags represent abstract types (2010-07-03)*

  The function ``Ada.Tags.Type_Is_Abstract`` returns ``True`` if invoked
  with the tag of an abstract type, and ``False`` otherwise.

  RM References:  3.09 (7.4/2)   3.09 (12.4/2)

.. index:: AI-0176 (Ada 2012 feature)

* *AI-0176 Quantified expressions (2010-09-29)*

  Both universally and existentially quantified expressions are implemented.
  They use the new syntax for iterators proposed in AI05-139-2, as well as
  the standard Ada loop syntax.

  RM References:  1.01.04 (12)   2.09 (2/2)   4.04 (7)   4.05.09 (0)

.. index:: AI-0177 (Ada 2012 feature)

* *AI-0177 Parameterized expressions (2010-07-10)*

  The new Ada 2012 notion of parameterized expressions is implemented. The form
  is:

  .. code-block:: ada

     function-specification is (expression)

  This is exactly equivalent to the
  corresponding function body that returns the expression, but it can appear
  in a package spec. Note that the expression must be parenthesized.

  RM References:  13.11.01 (3/2)

.. index:: AI-0178 (Ada 2012 feature)

* *AI-0178 Incomplete views are limited (0000-00-00)*

  This AI clarifies the role of incomplete views and plugs an omission in the
  RM. GNAT always correctly restricted the use of incomplete views and types.

  RM References:  7.05 (3/2)   7.05 (6/2)

.. index:: AI-0179 (Ada 2012 feature)

* *AI-0179 Statement not required after label (2010-04-10)*

  It is not necessary to have a statement following a label, so a label
  can appear at the end of a statement sequence without the need for putting a
  null statement afterwards, but it is not allowable to have only labels and
  no real statements in a statement sequence.

  RM References:  5.01 (2)

.. index:: AI-0181 (Ada 2012 feature)

* *AI-0181 Soft hyphen is a non-graphic character (2010-07-23)*

  From Ada 2005 on, soft hyphen is considered a non-graphic character, which
  means that it has a special name (``SOFT_HYPHEN``) in conjunction with the
  ``Image`` and ``Value`` attributes for the character types. Strictly
  speaking this is an inconsistency with Ada 95, but in practice the use of
  these attributes is so obscure that it will not cause problems.

  RM References:  3.05.02 (2/2)   A.01 (35/2)   A.03.03 (21)

.. index:: AI-0182 (Ada 2012 feature)

* *AI-0182 Additional forms for* ``Character'Value`` *(0000-00-00)*

  This AI allows ``Character'Value`` to accept the string ``'?'`` where
  ``?`` is any character including non-graphic control characters. GNAT has
  always accepted such strings. It also allows strings such as
  ``HEX_00000041`` to be accepted, but GNAT does not take advantage of this
  permission and raises ``Constraint_Error``, as is certainly still
  permitted.

  RM References:  3.05 (56/2)

.. index:: AI-0183 (Ada 2012 feature)

* *AI-0183 Aspect specifications (2010-08-16)*

  Aspect specifications have been fully implemented except for pre and post-
  conditions, and type invariants, which have their own separate AI's. All
  forms of declarations listed in the AI are supported. The following is a
  list of the aspects supported (with GNAT implementation aspects marked)

==================================== ===========
Supported Aspect                     Source
==================================== ===========
  ``Ada_2005``                           -- GNAT
  ``Ada_2012``                           -- GNAT
  ``Address``
  ``Alignment``
  ``Atomic``
  ``Atomic_Components``
  ``Bit_Order``
  ``Component_Size``
  ``Contract_Cases``                     -- GNAT
  ``Discard_Names``
  ``External_Tag``
  ``Favor_Top_Level``                    -- GNAT
  ``Inline``
  ``Inline_Always``                      -- GNAT
  ``Invariant``                          -- GNAT
  ``Machine_Radix``
  ``No_Return``
  ``Object_Size``                        -- GNAT
  ``Pack``
  ``Persistent_BSS``                     -- GNAT
  ``Post``
  ``Pre``
  ``Predicate``
  ``Preelaborable_Initialization``
  ``Pure_Function``                      -- GNAT
  ``Remote_Access_Type``                 -- GNAT
  ``Shared``                             -- GNAT
  ``Size``
  ``Storage_Pool``
  ``Storage_Size``
  ``Stream_Size``
  ``Suppress``
  ``Suppress_Debug_Info``                -- GNAT
  ``Test_Case``                          -- GNAT
  ``Thread_Local_Storage``               -- GNAT
  ``Type_Invariant``
  ``Unchecked_Union``
  ``Universal_Aliasing``                 -- GNAT
  ``Unmodified``                         -- GNAT
  ``Unreferenced``                       -- GNAT
  ``Unreferenced_Objects``               -- GNAT
  ``Unsuppress``
  ``Value_Size``                         -- GNAT
  ``Volatile``
  ``Volatile_Components``
  ``Warnings``                           -- GNAT
==================================== ===========

  Note that for aspects with an expression, e.g. ``Size``, the expression is
  treated like a default expression (visibility is analyzed at the point of
  occurrence of the aspect, but evaluation of the expression occurs at the
  freeze point of the entity involved).

  RM References:  3.02.01 (3)   3.02.02 (2)   3.03.01 (2/2)   3.08 (6)
  3.09.03 (1.1/2)   6.01 (2/2)   6.07 (2/2)   9.05.02 (2/2)   7.01 (3)   7.03
  (2)   7.03 (3)   9.01 (2/2)   9.01 (3/2)   9.04 (2/2)   9.04 (3/2)
  9.05.02 (2/2)   11.01 (2)   12.01 (3)   12.03 (2/2)   12.04 (2/2)   12.05 (2)
  12.06 (2.1/2)   12.06 (2.2/2)   12.07 (2)   13.01 (0.1/2)   13.03 (5/1)
  13.03.01 (0)

.. index:: AI-0185 (Ada 2012 feature)

* *AI-0185 Ada.Wide_[Wide_]Characters.Handling (2010-07-06)*

  Two new packages ``Ada.Wide_[Wide_]Characters.Handling`` provide
  classification functions for ``Wide_Character`` and
  ``Wide_Wide_Character``, as well as providing
  case folding routines for ``Wide_[Wide_]Character`` and
  ``Wide_[Wide_]String``.

  RM References:  A.03.05 (0)   A.03.06 (0)

.. index:: AI-0188 (Ada 2012 feature)

* *AI-0188 Case expressions (2010-01-09)*

  Case expressions are permitted. This allows use of constructs such as:

  .. code-block:: ada

      X := (case Y is when 1 => 2, when 2 => 3, when others => 31)

  RM References:  4.05.07 (0)   4.05.08 (0)   4.09 (12)   4.09 (33)

.. index:: AI-0189 (Ada 2012 feature)

* *AI-0189 No_Allocators_After_Elaboration (2010-01-23)*

  This AI introduces a new restriction ``No_Allocators_After_Elaboration``,
  which says that no dynamic allocation will occur once elaboration is
  completed.
  In general this requires a run-time check, which is not required, and which
  GNAT does not attempt. But the static cases of allocators in a task body or
  in the body of the main program are detected and flagged at compile or bind
  time.

  RM References:  D.07 (19.1/2)   H.04 (23.3/2)

.. index:: AI-0190 (Ada 2012 feature)

* *AI-0190 pragma Default_Storage_Pool (2010-09-15)*

  This AI introduces a new pragma ``Default_Storage_Pool``, which can be
  used to control storage pools globally.
  In particular, you can force every access
  type that is used for allocation (**new**) to have an explicit storage pool,
  or you can declare a pool globally to be used for all access types that lack
  an explicit one.

  RM References:  D.07 (8)

.. index:: AI-0193 (Ada 2012 feature)

* *AI-0193 Alignment of allocators (2010-09-16)*

  This AI introduces a new attribute ``Max_Alignment_For_Allocation``,
  analogous to ``Max_Size_In_Storage_Elements``, but for alignment instead
  of size.

  RM References:  13.11 (16)   13.11 (21)   13.11.01 (0)   13.11.01 (1)
  13.11.01 (2)   13.11.01 (3)

.. index:: AI-0194 (Ada 2012 feature)

* *AI-0194 Value of Stream_Size attribute (0000-00-00)*

  The ``Stream_Size`` attribute returns the default number of bits in the
  stream representation of the given type.
  This value is not affected by the presence
  of stream subprogram attributes for the type. GNAT has always implemented
  this interpretation.

  RM References:  13.13.02 (1.2/2)

.. index:: AI-0195 (Ada 2012 feature)

* *AI-0195 Invalid value handling is implementation defined (2010-07-03)*

  The handling of invalid values is now designated to be implementation
  defined. This is a documentation change only, requiring Annex M in the GNAT
  Reference Manual to document this handling.
  In GNAT, checks for invalid values are made
  only when necessary to avoid erroneous behavior. Operations like assignments
  which cannot cause erroneous behavior ignore the possibility of invalid
  values and do not do a check. The date given above applies only to the
  documentation change, this behavior has always been implemented by GNAT.

  RM References:  13.09.01 (10)

.. index:: AI-0196 (Ada 2012 feature)

* *AI-0196 Null exclusion tests for out parameters (0000-00-00)*

  Null exclusion checks are not made for ``out`` parameters when
  evaluating the actual parameters. GNAT has never generated these checks.

  RM References:  6.04.01 (13)

.. index:: AI-0198 (Ada 2012 feature)

* *AI-0198 Inheriting abstract operators  (0000-00-00)*

  This AI resolves a conflict between two rules involving inherited abstract
  operations and predefined operators. If a derived numeric type inherits
  an abstract operator, it overrides the predefined one. This interpretation
  was always the one implemented in GNAT.

  RM References:  3.09.03 (4/3)

.. index:: AI-0199 (Ada 2012 feature)

* *AI-0199 Aggregate with anonymous access components (2010-07-14)*

  A choice list in a record aggregate can include several components of
  (distinct) anonymous access types as long as they have matching designated
  subtypes.

  RM References:  4.03.01 (16)

.. index:: AI-0200 (Ada 2012 feature)

* *AI-0200 Mismatches in formal package declarations (0000-00-00)*

  This AI plugs a gap in the RM which appeared to allow some obviously intended
  illegal instantiations. GNAT has never allowed these instantiations.

  RM References:  12.07 (16)

.. index:: AI-0201 (Ada 2012 feature)

* *AI-0201 Independence of atomic object components (2010-07-22)*

  If an Atomic object has a pragma ``Pack`` or a ``Component_Size``
  attribute, then individual components may not be addressable by independent
  tasks. However, if the representation clause has no effect (is confirming),
  then independence is not compromised. Furthermore, in GNAT, specification of
  other appropriately addressable component sizes (e.g. 16 for 8-bit
  characters) also preserves independence. GNAT now gives very clear warnings
  both for the declaration of such a type, and for any assignment to its components.

  RM References:  9.10 (1/3)   C.06 (22/2)   C.06 (23/2)

.. index:: AI-0203 (Ada 2012 feature)

* *AI-0203 Extended return cannot be abstract (0000-00-00)*

  A return_subtype_indication cannot denote an abstract subtype. GNAT has never
  permitted such usage.

  RM References:  3.09.03 (8/3)

.. index:: AI-0205 (Ada 2012 feature)

* *AI-0205 Extended return declares visible name (0000-00-00)*

  This AI corrects a simple omission in the RM. Return objects have always
  been visible within an extended return statement.

  RM References:  8.03 (17)

.. index:: AI-0206 (Ada 2012 feature)

* *AI-0206 Remote types packages and preelaborate (2010-07-24)*

  Remote types packages are now allowed to depend on preelaborated packages.
  This was formerly considered illegal.

  RM References:  E.02.02 (6)

.. index:: AI-0207 (Ada 2012 feature)

* *AI-0207 Mode conformance and access constant (0000-00-00)*

  This AI confirms that access_to_constant indication must match for mode
  conformance. This was implemented in GNAT when the qualifier was originally
  introduced in Ada 2005.

  RM References:  6.03.01 (16/2)

.. index:: AI-0208 (Ada 2012 feature)

* *AI-0208 Characteristics of incomplete views (0000-00-00)*

  The wording in the Ada 2005 RM concerning characteristics of incomplete views
  was incorrect and implied that some programs intended to be legal were now
  illegal. GNAT had never considered such programs illegal, so it has always
  implemented the intent of this AI.

  RM References:  3.10.01 (2.4/2)   3.10.01 (2.6/2)

.. index:: AI-0210 (Ada 2012 feature)

* *AI-0210 Correct Timing_Events metric (0000-00-00)*

  This is a documentation only issue regarding wording of metric requirements,
  that does not affect the implementation of the compiler.

  RM References:  D.15 (24/2)

.. index:: AI-0211 (Ada 2012 feature)

* *AI-0211 No_Relative_Delays forbids Set_Handler use (2010-07-09)*

  The restriction ``No_Relative_Delays`` forbids any calls to the subprogram
  ``Ada.Real_Time.Timing_Events.Set_Handler``.

  RM References:  D.07 (5)   D.07 (10/2)   D.07 (10.4/2)   D.07 (10.7/2)

.. index:: AI-0214 (Ada 2012 feature)

* *AI-0214 Defaulted discriminants for limited tagged (2010-10-01)*

  Ada 2012 relaxes the restriction that forbids discriminants of tagged types
  to have default expressions by allowing them when the type is limited. It
  is often useful to define a default value for a discriminant even though
  it can't be changed by assignment.

  RM References:  3.07 (9.1/2)   3.07.02 (3)

.. index:: AI-0216 (Ada 2012 feature)

* *AI-0216 No_Task_Hierarchy forbids local tasks (0000-00-00)*

  It is clearly the intention that ``No_Task_Hierarchy`` is intended to
  forbid tasks declared locally within subprograms, or functions returning task
  objects, and that is the implementation that GNAT has always provided.
  However the language in the RM was not sufficiently clear on this point.
  Thus this is a documentation change in the RM only.

  RM References:  D.07 (3/3)

.. index:: AI-0219 (Ada 2012 feature)

* *AI-0219 Pure permissions and limited parameters (2010-05-25)*

  This AI refines the rules for the cases with limited parameters which do not
  allow the implementations to omit 'redundant'. GNAT now properly conforms
  to the requirements of this binding interpretation.

  RM References:  10.02.01 (18/2)

.. index:: AI-0220 (Ada 2012 feature)

* *AI-0220 Needed components for aggregates (0000-00-00)*

  This AI addresses a wording problem in the RM that appears to permit some
  complex cases of aggregates with nonstatic discriminants. GNAT has always
  implemented the intended semantics.

  RM References:  4.03.01 (17)
