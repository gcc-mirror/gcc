..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _non-fortran-main-program:

Non-Fortran Main Program
************************

.. toctree::
  :maxdepth: 2


Even if you are doing mixed-language programming, it is very
likely that you do not need to know or use the information in this
section.  Since it is about the internal structure of GNU Fortran,
it may also change in GCC minor releases.

When you compile a ``PROGRAM`` with GNU Fortran, a function
with the name ``main`` (in the symbol table of the object file)
is generated, which initializes the libgfortran library and then
calls the actual program which uses the name ``MAIN__``, for
historic reasons.  If you link GNU Fortran compiled procedures
to, e.g., a C or C++ program or to a Fortran program compiled by
a different compiler, the libgfortran library is not initialized
and thus a few intrinsic procedures do not work properly, e.g.
those for obtaining the command-line arguments.

Therefore, if your ``PROGRAM`` is not compiled with
GNU Fortran and the GNU Fortran compiled procedures require
intrinsics relying on the library initialization, you need to
initialize the library yourself.  Using the default options,
gfortran calls ``_gfortran_set_args`` and
``_gfortran_set_options``.  The initialization of the former
is needed if the called procedures access the command line
(and for backtracing); the latter sets some flags based on the
standard chosen or to enable backtracing.  In typical programs,
it is not necessary to call any initialization function.

If your ``PROGRAM`` is compiled with GNU Fortran, you shall
not call any of the following functions.  The libgfortran
initialization functions are shown in C syntax but using C
bindings they are also accessible from Fortran.

.. index:: _gfortran_set_args, libgfortran initialization, set_args

.. _gfortran_set_args:

_gfortran_set_args --- Save command-line arguments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_set_args (int argc, char *argv[])

  ``_gfortran_set_args`` saves the command-line arguments; this
  initialization is required if any of the command-line intrinsics
  is called.  Additionally, it shall be called if backtracing is
  enabled (see ``_gfortran_set_options``).

  :param argc:
    number of command line argument strings

  :param argv:
    the command-line argument strings; argv[0]
    is the pathname of the executable itself.

  :samp:`{Example}:`

  .. code-block:: c

      int main (int argc, char *argv[])
      {
        /* Initialize libgfortran.  */
        _gfortran_set_args (argc, argv);
        return 0;
      }

.. index:: _gfortran_set_options, libgfortran initialization, set_options

.. _gfortran_set_options:

_gfortran_set_options --- Set library option flags
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_set_options (int num, int options[])

  ``_gfortran_set_options`` sets several flags related to the Fortran
  standard to be used, whether backtracing should be enabled
  and whether range checks should be performed.  The syntax allows for
  upward compatibility since the number of passed flags is specified; for
  non-passed flags, the default value is used.  See also
  see :ref:`code-gen-options`.  Please note that not all flags are actually
  used.

  :param num:
    number of options passed

  :param argv:
    The list of flag values

  :samp:`{option flag list}:`

    .. list-table::
       :widths: 15 85

       * - :samp:`{option}` [0]
         - Allowed standard; can give run-time errors if e.g. an input-output edit descriptor is invalid in a given standard.  Possible values are (bitwise or-ed) ``GFC_STD_F77`` (1), ``GFC_STD_F95_OBS`` (2), ``GFC_STD_F95_DEL`` (4), ``GFC_STD_F95`` (8), ``GFC_STD_F2003`` (16), ``GFC_STD_GNU`` (32), ``GFC_STD_LEGACY`` (64), ``GFC_STD_F2008`` (128), ``GFC_STD_F2008_OBS`` (256), ``GFC_STD_F2008_TS`` (512), ``GFC_STD_F2018`` (1024), ``GFC_STD_F2018_OBS`` (2048), and ``GFC_STD=F2018_DEL`` (4096). Default: ``GFC_STD_F95_OBS | GFC_STD_F95_DEL | GFC_STD_F95 | GFC_STD_F2003 | GFC_STD_F2008 | GFC_STD_F2008_TS | GFC_STD_F2008_OBS | GFC_STD_F77 | GFC_STD_F2018 | GFC_STD_F2018_OBS | GFC_STD_F2018_DEL | GFC_STD_GNU | GFC_STD_LEGACY``.
       * - :samp:`{option}` [1]
         - Standard-warning flag; prints a warning to standard error.  Default: ``GFC_STD_F95_DEL | GFC_STD_LEGACY``.
       * - :samp:`{option}` [2]
         - If non zero, enable pedantic checking. Default: off.
       * - :samp:`{option}` [3]
         - Unused.
       * - :samp:`{option}` [4]
         - If non zero, enable backtracing on run-time errors.  Default: off. (Default in the compiler: on.) Note: Installs a signal handler and requires command-line initialization using ``_gfortran_set_args``.
       * - :samp:`{option}` [5]
         - If non zero, supports signed zeros. Default: enabled.
       * - :samp:`{option}` [6]
         - Enables run-time checking.  Possible values are (bitwise or-ed): GFC_RTCHECK_BOUNDS (1), GFC_RTCHECK_ARRAY_TEMPS (2), GFC_RTCHECK_RECURSION (4), GFC_RTCHECK_DO (8), GFC_RTCHECK_POINTER (16), GFC_RTCHECK_MEM (32), GFC_RTCHECK_BITS (64). Default: disabled.
       * - :samp:`{option}` [7]
         - Unused.
       * - :samp:`{option}` [8]
         - Show a warning when invoking ``STOP`` and ``ERROR STOP`` if a floating-point exception occurred. Possible values are (bitwise or-ed) ``GFC_FPE_INVALID`` (1), ``GFC_FPE_DENORMAL`` (2), ``GFC_FPE_ZERO`` (4), ``GFC_FPE_OVERFLOW`` (8), ``GFC_FPE_UNDERFLOW`` (16), ``GFC_FPE_INEXACT`` (32). Default: None (0). (Default in the compiler: ``GFC_FPE_INVALID | GFC_FPE_DENORMAL | GFC_FPE_ZERO | GFC_FPE_OVERFLOW | GFC_FPE_UNDERFLOW``.)

  :samp:`{Example}:`

    .. code-block:: c

        /* Use gfortran 4.9 default options.  */
        static int options[] = {68, 511, 0, 0, 1, 1, 0, 0, 31};
        _gfortran_set_options (9, &options);

.. index:: _gfortran_set_convert, libgfortran initialization, set_convert

.. _gfortran_set_convert:

_gfortran_set_convert --- Set endian conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_set_convert (int conv)

  ``_gfortran_set_convert`` set the representation of data for
  unformatted files.

  :param conv:
    Endian conversion, possible values:
    GFC_CONVERT_NATIVE (0, default), GFC_CONVERT_SWAP (1),
    GFC_CONVERT_BIG (2), GFC_CONVERT_LITTLE (3).

  :samp:`{Example}:`

    .. code-block:: c

      int main (int argc, char *argv[])
      {
        /* Initialize libgfortran.  */
        _gfortran_set_args (argc, argv);
        _gfortran_set_convert (1);
        return 0;
      }

.. index:: _gfortran_set_record_marker, libgfortran initialization, set_record_marker

.. _gfortran_set_record_marker:

_gfortran_set_record_marker --- Set length of record markers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_set_record_marker (int val)

  ``_gfortran_set_record_marker`` sets the length of record markers
  for unformatted files.

  :param val:
    Length of the record marker; valid values
    are 4 and 8.  Default is 4.

  :samp:`{Example}:`

    .. code-block:: c

      int main (int argc, char *argv[])
      {
        /* Initialize libgfortran.  */
        _gfortran_set_args (argc, argv);
        _gfortran_set_record_marker (8);
        return 0;
      }

.. index:: _gfortran_set_fpe, libgfortran initialization, set_fpe

.. _gfortran_set_fpe:

_gfortran_set_fpe --- Enable floating point exception traps
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_set_fpe (int val)

  ``_gfortran_set_fpe`` enables floating point exception traps for
  the specified exceptions.  On most systems, this will result in a
  SIGFPE signal being sent and the program being aborted.

  :param option} [0]:
    IEEE exceptions.  Possible values are
    (bitwise or-ed) zero (0, default) no trapping,
    ``GFC_FPE_INVALID`` (1), ``GFC_FPE_DENORMAL`` (2),
    ``GFC_FPE_ZERO`` (4), ``GFC_FPE_OVERFLOW`` (8),
    ``GFC_FPE_UNDERFLOW`` (16), and ``GFC_FPE_INEXACT`` (32).

  :samp:`{Example}:`

    .. code-block:: c

      int main (int argc, char *argv[])
      {
        /* Initialize libgfortran.  */
        _gfortran_set_args (argc, argv);
        /* FPE for invalid operations such as SQRT(-1.0).  */
        _gfortran_set_fpe (1);
        return 0;
      }

.. index:: _gfortran_set_max_subrecord_length, libgfortran initialization, set_max_subrecord_length

.. _gfortran_set_max_subrecord_length:

_gfortran_set_max_subrecord_length --- Set subrecord length
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void _gfortran_set_max_subrecord_length (int val)

  ``_gfortran_set_max_subrecord_length`` set the maximum length
  for a subrecord.  This option only makes sense for testing and
  debugging of unformatted I/O.

  :param val:
    the maximum length for a subrecord;
    the maximum permitted value is 2147483639, which is also
    the default.

  :samp:`{Example}:`

    .. code-block:: c

      int main (int argc, char *argv[])
      {
        /* Initialize libgfortran.  */
        _gfortran_set_args (argc, argv);
        _gfortran_set_max_subrecord_length (8);
        return 0;
      }