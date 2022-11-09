..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _option-properties:

Option properties
*****************

The second field of an option record can specify any of the following
properties.  When an option takes an argument, it is enclosed in parentheses
following the option property name.  The parser that handles option files
is quite simplistic, and will be tricked by any nested parentheses within
the argument text itself; in this case, the entire option argument can
be wrapped in curly braces within the parentheses to demarcate it, e.g.:

.. code-block:: c++

  Condition({defined (USE_CYGWIN_LIBSTDCXX_WRAPPERS)})

``Common``
  The option is available for all languages and targets.

``Target``
  The option is available for all languages but is target-specific.

``Driver``
  The option is handled by the compiler driver using code not shared
  with the compilers proper (:samp:`cc1` etc.).

``language``
  The option is available when compiling for the given language.

  It is possible to specify several different languages for the same
  option.  Each :samp:`{language}` must have been declared by an earlier
  ``Language`` record.  See :ref:`option-file-format`.

``RejectDriver``
  The option is only handled by the compilers proper (:samp:`cc1` etc.)
  and should not be accepted by the driver.

``RejectNegative``
  The option does not have a 'no-' form.  All options beginning with
  'f', 'W' or 'm' are assumed to have a 'no-' form unless this
  property is used.

:samp:`Negative({othername})`
  The option will turn off another option :samp:`{othername}`, which is
  the option name with the leading '-' removed.  This chain action will
  propagate through the ``Negative`` property of the option to be
  turned off.  The driver will prune options, removing those that are
  turned off by some later option.  This pruning is not done for options
  with ``Joined`` or ``JoinedOrMissing`` properties, unless the
  options have both the ``RejectNegative`` property and the ``Negative``
  property mentions itself.

  As a consequence, if you have a group of mutually-exclusive
  options, their ``Negative`` properties should form a circular chain.
  For example, if options :option:`-a`, :option:`-b` and
  :option:`-c` are mutually exclusive, their respective ``Negative``
  properties should be :samp:`Negative({b})`, :samp:`Negative({c})`
  and :samp:`Negative({a})`.

``Joined`` ``Separate``
  The option takes a mandatory argument.  ``Joined`` indicates
  that the option and argument can be included in the same ``argv``
  entry (as with ``-mflush-func=name``, for example).
  ``Separate`` indicates that the option and argument can be
  separate ``argv`` entries (as with ``-o``).  An option is
  allowed to have both of these properties.

``JoinedOrMissing``
  The option takes an optional argument.  If the argument is given,
  it will be part of the same ``argv`` entry as the option itself.

  This property cannot be used alongside ``Joined`` or ``Separate``.

:samp:`MissingArgError({message})`
  For an option marked ``Joined`` or ``Separate``, the message
  :samp:`{message}` will be used as an error message if the mandatory
  argument is missing; for options without ``MissingArgError``, a
  generic error message is used.  :samp:`{message}` should contain a single
  :samp:`%qs` format, which will be used to format the name of the option
  passed.

:samp:`Args({n})`
  For an option marked ``Separate``, indicate that it takes :samp:`{n}`
  arguments.  The default is 1.

``UInteger``
  The option's argument is a non-negative integer consisting of either
  decimal or hexadecimal digits interpreted as ``int``.  Hexadecimal
  integers may optionally start with the ``0x`` or ``0X`` prefix.
  The option parser validates and converts the argument before passing
  it to the relevant option handler.  ``UInteger`` should also be used
  with options like ``-falign-loops`` where both ``-falign-loops``
  and ``-falign-loops`` = :samp:`{n}` are supported to make sure the saved
  options are given a full integer.  Positive values of the argument in
  excess of ``INT_MAX`` wrap around zero.

``Host_Wide_Int``
  The option's argument is a non-negative integer consisting of either
  decimal or hexadecimal digits interpreted as the widest integer type
  on the host.  As with an ``UInteger`` argument, hexadecimal integers
  may optionally start with the ``0x`` or ``0X`` prefix.  The option
  parser validates and converts the argument before passing it to
  the relevant option handler.  ``Host_Wide_Int`` should be used with
  options that need to accept very large values.  Positive values of
  the argument in excess of ``HOST_WIDE_INT_M1U`` are assigned
  ``HOST_WIDE_INT_M1U``.

:samp:`IntegerRange({n}, {m})`
  The options's arguments are integers of type ``int``.  The option's
  parser validates that the value of an option integer argument is within
  the closed range [ :samp:`{n}`, :samp:`{m}` ].

``ByteSize``
  A property applicable only to ``UInteger`` or ``Host_Wide_Int``
  arguments.  The option's integer argument is interpreted as if in infinite
  precision using saturation arithmetic in the corresponding type.  The argument
  may be followed by a :samp:`byte-size` suffix designating a multiple of bytes
  such as ``kB`` and ``KiB`` for kilobyte and kibibyte, respectively,
  ``MB`` and ``MiB`` for megabyte and mebibyte, ``GB`` and ``GiB``
  for gigabyte and gigibyte, and so on.  ``ByteSize`` should be used for
  with options that take a very large argument representing a size in bytes,
  such as :option:`-Wlarger-than=`.

``ToLower``
  The option's argument should be converted to lowercase as part of
  putting it in canonical form, and before comparing with the strings
  indicated by any ``Enum`` property.

``NoDriverArg``
  For an option marked ``Separate``, the option only takes an
  argument in the compiler proper, not in the driver.  This is for
  compatibility with existing options that are used both directly and
  via :option:`-Wp,` ; new options should not have this property.

:samp:`Var({var})`
  The state of this option should be stored in variable :samp:`{var}`
  (actually a macro for ``global_options.x_var``).
  The way that the state is stored depends on the type of option:

``WarnRemoved``
  The option is removed and every usage of such option will
  result in a warning.  We use it option backward compatibility.

:samp:`Var({var}, {set})`
  The option controls an integer variable :samp:`{var}` and is active when
  :samp:`{var}` equals :samp:`{set}`.  The option parser will set :samp:`{var}` to
  :samp:`{set}` when the positive form of the option is used and ``!set``
  when the 'no-' form is used.

  :samp:`{var}` is declared in the same way as for the single-argument form
  described above.

  * If the option uses the ``Mask`` or ``InverseMask`` properties,
    :samp:`{var}` is the integer variable that contains the mask.

  * If the option is a normal on/off switch, :samp:`{var}` is an integer
    variable that is nonzero when the option is enabled.  The options
    parser will set the variable to 1 when the positive form of the
    option is used and 0 when the 'no-' form is used.

  * If the option takes an argument and has the ``UInteger`` property,
    :samp:`{var}` is an integer variable that stores the value of the argument.

  * If the option takes an argument and has the ``Enum`` property,
    :samp:`{var}` is a variable (type given in the ``Type`` property of the
    :samp:`Enum` record whose ``Name`` property has the same argument as
    the ``Enum`` property of this option) that stores the value of the
    argument.

  * If the option has the ``Defer`` property, :samp:`{var}` is a pointer to
    a ``VEC(cl_deferred_option,heap)`` that stores the option for later
    processing.  (:samp:`{var}` is declared with type ``void *`` and needs
    to be cast to ``VEC(cl_deferred_option,heap)`` before use.)

  * Otherwise, if the option takes an argument, :samp:`{var}` is a pointer to
    the argument string.  The pointer will be null if the argument is optional
    and wasn't given.

  The option-processing script will usually zero-initialize :samp:`{var}`.
  You can modify this behavior using ``Init``.

:samp:`Init({value})`
  The variable specified by the ``Var`` property should be statically
  initialized to :samp:`{value}`.  If more than one option using the same
  variable specifies ``Init``, all must specify the same initializer.

:samp:`Mask({name})`
  The option is associated with a bit in the ``target_flags``
  variable (see :ref:`run-time-target`) and is active when that bit is set.
  You may also specify ``Var`` to select a variable other than
  ``target_flags``.

  The options-processing script will automatically allocate a unique bit
  for the option.  If the option is attached to :samp:`target_flags`,
  the script will set the macro ``MASK_name`` to the appropriate
  bitmask.  It will also declare a ``TARGET_name`` macro that has
  the value 1 when the option is active and 0 otherwise.  If you use ``Var``
  to attach the option to a different variable, the bitmask macro with be
  called ``OPTION_MASK_name``.

:samp:`InverseMask({othername})` :samp:`InverseMask({othername}, {thisname})`
  The option is the inverse of another option that has the
  ``Mask(othername)`` property.  If :samp:`{thisname}` is given,
  the options-processing script will declare a ``TARGET_thisname``
  macro that is 1 when the option is active and 0 otherwise.

:samp:`Enum({name})`
  The option's argument is a string from the set of strings associated
  with the corresponding :samp:`Enum` record.  The string is checked and
  converted to the integer specified in the corresponding
  :samp:`EnumValue` record before being passed to option handlers.

``EnumSet``
  Must be used together with the ``Enum(name)`` property.
  Corresponding :samp:`Enum` record must use ``Set`` properties.
  The option's argument is either a string from the set like for
  ``Enum(name)``, but with a slightly different behavior that
  the whole ``Var`` isn't overwritten, but only the bits in all the
  enumeration values with the same set bitwise ored together.
  Or option's argument can be a comma separated list of strings where
  each string is from a different ``Set(number)``.

``EnumBitSet``
  Must be used together with the ``Enum(name)`` property.
  Similar to :samp:`EnumSet`, but corresponding :samp:`Enum` record must
  not use ``Set`` properties, each ``EnumValue`` should have
  ``Value`` that is a power of 2, each value is treated as its own
  set and its value as the set's mask, so there are no mutually
  exclusive arguments.

``Defer``
  The option should be stored in a vector, specified with ``Var``,
  for later processing.

:samp:`Alias({opt})` :samp:`Alias({opt}, {arg})` :samp:`Alias({opt}, {posarg}, {negarg})`
  The option is an alias for :option:`-opt` (or the negative form
  of that option, depending on ``NegativeAlias``).  In the first form,
  any argument passed to the alias is considered to be passed to
  :option:`-opt`, and :option:`-opt` is considered to be
  negated if the alias is used in negated form.  In the second form, the
  alias may not be negated or have an argument, and :samp:`{posarg}` is
  considered to be passed as an argument to :option:`-opt`.  In the
  third form, the alias may not have an argument, if the alias is used
  in the positive form then :samp:`{posarg}` is considered to be passed to
  :option:`-opt`, and if the alias is used in the negative form
  then :samp:`{negarg}` is considered to be passed to :option:`-opt`.

  Aliases should not specify ``Var`` or ``Mask`` or
  ``UInteger``.  Aliases should normally specify the same languages
  as the target of the alias; the flags on the target will be used to
  determine any diagnostic for use of an option for the wrong language,
  while those on the alias will be used to identify what command-line
  text is the option and what text is any argument to that option.

  When an ``Alias`` definition is used for an option, driver specs do
  not need to handle it and no :samp:`OPT_` enumeration value is defined
  for it; only the canonical form of the option will be seen in those
  places.

``NegativeAlias``
  For an option marked with ``Alias(opt)``, the option is
  considered to be an alias for the positive form of :option:`-opt`
  if negated and for the negative form of :option:`-opt` if not
  negated.  ``NegativeAlias`` may not be used with the forms of
  ``Alias`` taking more than one argument.

``Ignore``
  This option is ignored apart from printing any warning specified using
  ``Warn``.  The option will not be seen by specs and no :samp:`OPT_`
  enumeration value is defined for it.

``SeparateAlias``
  For an option marked with ``Joined``, ``Separate`` and
  ``Alias``, the option only acts as an alias when passed a separate
  argument; with a joined argument it acts as a normal option, with an
  :samp:`OPT_` enumeration value.  This is for compatibility with the
  Java :option:`-d` option and should not be used for new options.

:samp:`Warn({message})`
  If this option is used, output the warning :samp:`{message}`.
  :samp:`{message}` is a format string, either taking a single operand with
  a :samp:`%qs` format which is the option name, or not taking any
  operands, which is passed to the :samp:`warning` function.  If an alias
  is marked ``Warn``, the target of the alias must not also be marked
  ``Warn``.

``Warning``
  This is a warning option and should be shown as such in
  :option:`--help` output.  This flag does not currently affect anything
  other than :option:`--help`.

``Optimization``
  This is an optimization option.  It should be shown as such in
  :option:`--help` output, and any associated variable named using
  ``Var`` should be saved and restored when the optimization level is
  changed with ``optimize`` attributes.

``PerFunction``
  This is an option that can be overridden on a per-function basis.
  ``Optimization`` implies ``PerFunction``, but options that do not
  affect executable code generation may use this flag instead, so that the
  option is not taken into account in ways that might affect executable
  code generation.

``Param``
  This is an option that is a parameter.

``Undocumented``
  The option is deliberately missing documentation and should not
  be included in the :option:`--help` output.

:samp:`Condition({cond})`
  The option should only be accepted if preprocessor condition
  :samp:`{cond}` is true.  Note that any C declarations associated with the
  option will be present even if :samp:`{cond}` is false; :samp:`{cond}` simply
  controls whether the option is accepted and whether it is printed in
  the :option:`--help` output.

``Save``
  Build the ``cl_target_option`` structure to hold a copy of the
  option, add the functions ``cl_target_option_save`` and
  ``cl_target_option_restore`` to save and restore the options.

``SetByCombined``
  The option may also be set by a combined option such as
  :option:`-ffast-math`.  This causes the ``gcc_options`` struct to
  have a field ``frontend_set_name``, where ``name``
  is the name of the field holding the value of this option (without the
  leading ``x_``).  This gives the front end a way to indicate that
  the value has been set explicitly and should not be changed by the
  combined option.  For example, some front ends use this to prevent
  :option:`-ffast-math` and :option:`-fno-fast-math` from changing the
  value of :option:`-fmath-errno` for languages that do not use
  ``errno``.

:samp:`EnabledBy({opt})` :samp:`EnabledBy({opt} || {opt2})` :samp:`EnabledBy({opt} && {opt2})`
  If not explicitly set, the option is set to the value of
  :option:`-opt` ; multiple options can be given, separated by
  ``||``.  The third form using ``&&`` specifies that the option is
  only set if both :samp:`{opt}` and :samp:`{opt2}` are set. The options :samp:`{opt}`
  and :samp:`{opt2}` must have the ``Common`` property; otherwise, use
  ``LangEnabledBy``.

:samp:`LangEnabledBy({language}, {opt})` :samp:`LangEnabledBy({language}, {opt}, {posarg}, {negarg})`
  When compiling for the given language, the option is set to the value
  of :option:`-opt`, if not explicitly set. :samp:`{opt}` can be also a list
  of ``||`` separated options. In the second form, if
  :samp:`{opt}` is used in the positive form then :samp:`{posarg}` is considered
  to be passed to the option, and if :samp:`{opt}` is used in the negative
  form then :samp:`{negarg}` is considered to be passed to the option.  It
  is possible to specify several different languages.  Each
  :samp:`{language}` must have been declared by an earlier ``Language``
  record.  See :ref:`option-file-format`.

``NoDWARFRecord``
  The option is omitted from the producer string written by
  :option:`-grecord-gcc-switches`.

``PchIgnore``
  Even if this is a target option, this option will not be recorded / compared
  to determine if a precompiled header file matches.

:samp:`CPP({var})`
  The state of this option should be kept in sync with the preprocessor
  option :samp:`{var}`.  If this property is set, then properties ``Var``
  and ``Init`` must be set as well.

:samp:`CppReason({CPP_W_Enum})`
  This warning option corresponds to ``cpplib.h`` warning reason code
  :samp:`{CPP_W_Enum}`.  This should only be used for warning options of the
  C-family front-ends.
