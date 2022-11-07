..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: optimization dumps

.. _optimization-info:

Optimization info
*****************

This section is describes dump infrastructure which is common to both
pass dumps as well as optimization dumps. The goal for this
infrastructure is to provide both gcc developers and users detailed
information about various compiler transformations and optimizations.

.. toctree::
  :maxdepth: 2


.. index:: dump setup

.. _dump-setup:

Dump setup
^^^^^^^^^^

A dump_manager class is defined in :samp:`dumpfile.h`. Various passes
register dumping pass-specific information via ``dump_register`` in
:samp:`passes.cc`. During the registration, an optimization pass can
select its optimization group (see :ref:`optimization-groups`). After
that optimization information corresponding to the entire group
(presumably from multiple passes) can be output via command-line
switches. Note that if a pass does not fit into any of the pre-defined
groups, it can select ``OPTGROUP_NONE``.

Note that in general, a pass need not know its dump output file name,
whether certain flags are enabled, etc. However, for legacy reasons,
passes could also call ``dump_begin`` which returns a stream in
case the particular pass has optimization dumps enabled. A pass could
call ``dump_end`` when the dump has ended. These methods should go
away once all the passes are converted to use the new dump
infrastructure.

The recommended way to setup the dump output is via ``dump_start``
and ``dump_end``.

.. index:: optimization groups

.. _optimization-groups:

Optimization groups
^^^^^^^^^^^^^^^^^^^

The optimization passes are grouped into several categories. Currently
defined categories in :samp:`dumpfile.h` are

.. envvar:: OPTGROUP_IPA

  IPA optimization passes. Enabled by :option:`-ipa`

.. envvar:: OPTGROUP_LOOP

  Loop optimization passes. Enabled by :option:`-loop`.

.. envvar:: OPTGROUP_INLINE

  Inlining passes. Enabled by :option:`-inline`.

.. envvar:: OPTGROUP_OMP

  OMP (Offloading and Multi Processing) passes. Enabled by
  :option:`-omp`.

.. envvar:: OPTGROUP_VEC

  Vectorization passes. Enabled by :option:`-vec`.

.. envvar:: OPTGROUP_OTHER

  All other optimization passes which do not fall into one of the above.

.. envvar:: OPTGROUP_ALL

  All optimization passes. Enabled by :option:`-optall`.

By using groups a user could selectively enable optimization
information only for a group of passes. By default, the optimization
information for all the passes is dumped.

.. index:: optimization info file names

.. _dump-files-and-streams:

Dump files and streams
^^^^^^^^^^^^^^^^^^^^^^

There are two separate output streams available for outputting
optimization information from passes. Note that both these streams
accept ``stderr`` and ``stdout`` as valid streams and thus it is
possible to dump output to standard output or error. This is specially
handy for outputting all available information in a single file by
redirecting ``stderr``.

``pstream``
  This stream is for pass-specific dump output. For example,
  :option:`-fdump-tree-vect=foo.v` dumps tree vectorization pass output
  into the given file name :samp:`foo.v`. If the file name is not provided,
  the default file name is based on the source file and pass number. Note
  that one could also use special file names ``stdout`` and
  ``stderr`` for dumping to standard output and standard error
  respectively.

``alt_stream``
  This steam is used for printing optimization specific output in
  response to the :option:`-fopt-info`. Again a file name can be given. If
  the file name is not given, it defaults to ``stderr``.

.. index:: dump verbosity

.. _dump-output-verbosity:

Dump output verbosity
^^^^^^^^^^^^^^^^^^^^^

The dump verbosity has the following options

:samp:`optimized`
  Print information when an optimization is successfully applied. It is
  up to a pass to decide which information is relevant. For example, the
  vectorizer passes print the source location of loops which got
  successfully vectorized.

:samp:`missed`
  Print information about missed optimizations. Individual passes
  control which information to include in the output. For example,

  .. code-block:: shell

    gcc -O2 -ftree-vectorize -fopt-info-vec-missed

  will print information about missed optimization opportunities from
  vectorization passes on stderr.

:samp:`note`
  Print verbose information about optimizations, such as certain
  transformations, more detailed messages about decisions etc.

:samp:`all`
  Print detailed optimization information. This includes
  :samp:`{optimized}`, :samp:`{missed}`, and :samp:`{note}`.

.. index:: dump types

.. _dump-types:

Dump types
^^^^^^^^^^

``dump_printf``
  This is a generic method for doing formatted output. It takes an
  additional argument ``dump_kind`` which signifies the type of
  dump. This method outputs information only when the dumps are enabled
  for this particular ``dump_kind``. Note that the caller doesn't
  need to know if the particular dump is enabled or not, or even the
  file name. The caller only needs to decide which dump output
  information is relevant, and under what conditions. This determines
  the associated flags.

  Consider the following example from :samp:`loop-unroll.cc` where an
  informative message about a loop (along with its location) is printed
  when any of the following flags is enabled

  * optimization messages

  * RTL dumps

  * detailed dumps

  .. code-block:: c++

    int report_flags = MSG_OPTIMIZED_LOCATIONS | TDF_RTL | TDF_DETAILS;
    dump_printf_loc (report_flags, insn,
                     "loop turned into non-loop; it never loops.\n");

``dump_basic_block``
  Output basic block.

``dump_generic_expr``
  Output generic expression.

``dump_gimple_stmt``
  Output gimple statement.

  Note that the above methods also have variants prefixed with
  ``_loc``, such as ``dump_printf_loc``, which are similar except
  they also output the source location information.  The ``_loc`` variants
  take a ``const dump_location_t &``.  This class can be constructed from
  a ``gimple *`` or from a ``rtx_insn *``, and so callers can pass
  a ``gimple *`` or a ``rtx_insn *`` as the ``_loc`` argument.
  The ``dump_location_t`` constructor will extract the source location
  from the statement or instruction, along with the profile count, and
  the location in GCC's own source code (or the plugin) from which the dump
  call was emitted.  Only the source location is currently used.
  There is also a ``dump_user_location_t`` class, capturing the
  source location and profile count, but not the dump emission location,
  so that locations in the user's code can be passed around.  This
  can also be constructed from a ``gimple *`` and from a ``rtx_insn *``,
  and it too can be passed as the ``_loc`` argument.

.. index:: dump examples

.. _dump-examples:

Dump examples
^^^^^^^^^^^^^

.. code-block:: shell

  gcc -O3 -fopt-info-missed=missed.all

outputs missed optimization report from all the passes into
:samp:`missed.all`.

As another example,

.. code-block:: shell

  gcc -O3 -fopt-info-inline-optimized-missed=inline.txt

will output information about missed optimizations as well as
optimized locations from all the inlining passes into
:samp:`inline.txt`.

If the :samp:`{filename}` is provided, then the dumps from all the
applicable optimizations are concatenated into the :samp:`filename`.
Otherwise the dump is output onto :samp:`stderr`. If :samp:`{options}` is
omitted, it defaults to optimized-optall, which means dump
all information about successful optimizations from all the passes.
In the following example, the optimization information is output on
to :samp:`stderr`.

.. code-block:: shell

  gcc -O3 -fopt-info

Note that :option:`-fopt-info-vec-missed` behaves the same as
:option:`-fopt-info-missed-vec`.  The order of the optimization group
names and message types listed after :option:`-fopt-info` does not matter.

As another example, consider

.. code-block:: shell

  gcc -fopt-info-vec-missed=vec.miss -fopt-info-loop-optimized=loop.opt

Here the two output file names :samp:`vec.miss` and :samp:`loop.opt` are
in conflict since only one output file is allowed. In this case, only
the first option takes effect and the subsequent options are
ignored. Thus only the :samp:`vec.miss` is produced which containts
dumps from the vectorizer about missed opportunities.