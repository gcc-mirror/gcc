..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _option-file-format:

Option file format
******************

Option files are a simple list of records in which each field occupies
its own line and in which the records themselves are separated by
blank lines.  Comments may appear on their own line anywhere within
the file and are preceded by semicolons.  Whitespace is allowed before
the semicolon.

The files can contain the following types of record:

* A language definition record.  These records have two fields: the
  string :samp:`Language` and the name of the language.  Once a language
  has been declared in this way, it can be used as an option property.
  See :ref:`option-properties`.

* A target specific save record to save additional information. These
  records have two fields: the string :samp:`TargetSave`, and a
  declaration type to go in the ``cl_target_option`` structure.

* A variable record to define a variable used to store option
  information.  These records have two fields: the string
  :samp:`Variable`, and a declaration of the type and name of the
  variable, optionally with an initializer (but without any trailing
  :samp:`;`).  These records may be used for variables used for many
  options where declaring the initializer in a single option definition
  record, or duplicating it in many records, would be inappropriate, or
  for variables set in option handlers rather than referenced by
  ``Var`` properties.

* A variable record to define a variable used to store option
  information.  These records have two fields: the string
  :samp:`TargetVariable`, and a declaration of the type and name of the
  variable, optionally with an initializer (but without any trailing
  :samp:`;`).  :samp:`TargetVariable` is a combination of :samp:`Variable`
  and :samp:`TargetSave` records in that the variable is defined in the
  ``gcc_options`` structure, but these variables are also stored in
  the ``cl_target_option`` structure.  The variables are saved in the
  target save code and restored in the target restore code.

* A variable record to record any additional files that the
  :samp:`options.h` file should include.  This is useful to provide
  enumeration or structure definitions needed for target variables.
  These records have two fields: the string :samp:`HeaderInclude` and the
  name of the include file.

* A variable record to record any additional files that the
  :samp:`options.cc` or :samp:`options-save.cc` file should include.  This
  is useful to provide
  inline functions needed for target variables and/or ``#ifdef``
  sequences to properly set up the initialization.  These records have
  two fields: the string :samp:`SourceInclude` and the name of the
  include file.

* An enumeration record to define a set of strings that may be used as
  arguments to an option or options.  These records have three fields:
  the string :samp:`Enum`, a space-separated list of properties and help
  text used to describe the set of strings in :option:`--help` output.
  Properties use the same format as option properties; the following are
  valid:

  :samp:`Name({name})`
    This property is required; :samp:`{name}` must be a name (suitable for use
    in C identifiers) used to identify the set of strings in ``Enum``
    option properties.

  :samp:`Type({type})`
    This property is required; :samp:`{type}` is the C type for variables set
    by options using this enumeration together with ``Var``.

  :samp:`UnknownError({message})`
    The message :samp:`{message}` will be used as an error message if the
    argument is invalid; for enumerations without ``UnknownError``, a
    generic error message is used.  :samp:`{message}` should contain a single
    :samp:`%qs` format, which will be used to format the invalid argument.

* An enumeration value record to define one of the strings in a set
  given in an :samp:`Enum` record.  These records have two fields: the
  string :samp:`EnumValue` and a space-separated list of properties.
  Properties use the same format as option properties; the following are
  valid:

  :samp:`Enum({name})`
    This property is required; :samp:`{name}` says which :samp:`Enum` record
    this :samp:`EnumValue` record corresponds to.

  :samp:`String({string})`
    This property is required; :samp:`{string}` is the string option argument
    being described by this record.

  :samp:`Value({value})`
    This property is required; it says what value (representable as
    ``int``) should be used for the given string.

  ``Canonical``
    This property is optional.  If present, it says the present string is
    the canonical one among all those with the given value.  Other strings
    yielding that value will be mapped to this one so specs do not need to
    handle them.

  ``DriverOnly``
    This property is optional.  If present, the present string will only
    be accepted by the driver.  This is used for cases such as
    :option:`-march=native` that are processed by the driver so that
    :samp:`gcc -v` shows how the options chosen depended on the system on
    which the compiler was run.

  :samp:`Set({number})`
    This property is optional, required for enumerations used in
    ``EnumSet`` options.  :samp:`{number}` should be decimal number between
    1 and 64 inclusive and divides the enumeration into a set of
    sets of mutually exclusive arguments.  Arguments with the same
    :samp:`{number}` can't be specified together in the same option, but
    arguments with different :samp:`{number}` can.  :samp:`{value}` needs to be
    chosen such that a mask of all :samp:`{value}` values from the same set
    :samp:`{number}` bitwise ored doesn't overlap with masks for other sets.
    When ``-foption=arg_from_set1,arg_from_set4`` and
    ``-fno-option=arg_from_set3`` are used, the effect is that previous
    value of the ``Var`` will get bits from set 1 and 4 masks cleared,
    ored ``Value`` of ``arg_from_set1`` and ``arg_from_set4``
    and then will get bits from set 3 mask cleared.

* An option definition record.  These records have the following fields:

  * the name of the option, with the leading '-' removed

  * a space-separated list of option properties (see :ref:`option-properties`)

  * the help text to use for :option:`--help` (omitted if the second field
    contains the ``Undocumented`` property).

  By default, all options beginning with 'f', 'W' or 'm' are
  implicitly assumed to take a 'no-' form.  This form should not be
  listed separately.  If an option beginning with one of these letters
  does not have a 'no-' form, you can use the ``RejectNegative``
  property to reject it.

  The help text is automatically line-wrapped before being displayed.
  Normally the name of the option is printed on the left-hand side of
  the output and the help text is printed on the right.  However, if the
  help text contains a tab character, the text to the left of the tab is
  used instead of the option's name and the text to the right of the
  tab forms the help text.  This allows you to elaborate on what type
  of argument the option takes.

  There is no support for different help texts for different languages.
  If an option is supported for multiple languages, use a generic
  description that is correct for all of them.

  If an option has multiple option definition records (in different
  front ends' :samp:`*.opt` files, and/or :samp:`gcc/common.opt`, for
  example), convention is to not duplicate the help text for each of
  them, but instead put a comment like ``; documented in common.opt``
  in place of the help text for all but one of the multiple option
  definition records.

* A target mask record.  These records have one field of the form
  :samp:`Mask({x})`.  The options-processing script will automatically
  allocate a bit in ``target_flags`` (see :ref:`run-time-target`) for
  each mask name :samp:`{x}` and set the macro ``MASK_x`` to the
  appropriate bitmask.  It will also declare a ``TARGET_x``
  macro that has the value 1 when bit ``MASK_x`` is set and
  0 otherwise.

  They are primarily intended to declare target masks that are not
  associated with user options, either because these masks represent
  internal switches or because the options are not available on all
  configurations and yet the masks always need to be defined.
