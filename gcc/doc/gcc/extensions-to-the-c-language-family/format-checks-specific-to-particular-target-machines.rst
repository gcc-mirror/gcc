..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _target-format-checks:

Format Checks Specific to Particular Target Machines
****************************************************

For some target machines, GCC supports additional options to the
format attribute
(see :ref:`function-attributes`).

.. toctree::
  :maxdepth: 2


.. _solaris-format-checks:

Solaris Format Checks
^^^^^^^^^^^^^^^^^^^^^

Solaris targets support the ``cmn_err`` (or ``__cmn_err__``) format
check.  ``cmn_err`` accepts a subset of the standard ``printf``
conversions, and the two-argument ``%b`` conversion for displaying
bit-fields.  See the Solaris man page for ``cmn_err`` for more information.

.. _darwin-format-checks:

Darwin Format Checks
^^^^^^^^^^^^^^^^^^^^

In addition to the full set of format archetypes (attribute format style
arguments such as ``printf``, ``scanf``, ``strftime``, and
``strfmon``), Darwin targets also support the ``CFString`` (or
``__CFString__``) archetype in the ``format`` attribute.
Declarations with this archetype are parsed for correct syntax
and argument types.  However, parsing of the format string itself and
validating arguments against it in calls to such functions is currently
not performed.

Additionally, ``CFStringRefs`` (defined by the ``CoreFoundation`` headers) may
also be used as format arguments.  Note that the relevant headers are only likely to be
available on Darwin (OSX) installations.  On such installations, the XCode and system
documentation provide descriptions of ``CFString``, ``CFStringRefs`` and
associated functions.