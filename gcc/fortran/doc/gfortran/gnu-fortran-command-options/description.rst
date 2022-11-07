.. only:: man

  Synopsis
  ^^^^^^^^

  gfortran [ :option:`-c` | :option:`-S` | :option:`-E` ]
           [ :option:`-g` ] [ :option:`-pg` ] [ :option:`-O`:samp:`{level}` ]
           [ :option:`-W`:samp:`{warn}`...] [ :option:`-pedantic` ]
           [ :option:`-I`:samp:`{dir}`...] [ :option:`-L`:samp:`{dir}`...]
           [ :option:`-D`:samp:`{macro}` [= :samp:`{defn}` ]...] [ :option:`-U`:samp:`{macro}` ]
           [ :option:`-f`:samp:`{option}`...]
           [ :option:`-m`:samp:`{machine-option}`...]
           [ :option:`-o` :samp:`{outfile}` ] :samp:`{infile}`...

Description
^^^^^^^^^^^

The :command:`gfortran` command supports all the options supported by the
:command:`gcc` command.  Only options specific to GNU Fortran are documented
here.

See :ref:`gcc:invoking-gcc`, for information
on the non-Fortran-specific aspects of the :command:`gcc` command (and,
therefore, the :command:`gfortran` command).

.. index:: options, negative forms

All GCC and GNU Fortran options
are accepted both by :command:`gfortran` and by :command:`gcc`
(as well as any other drivers built at the same time,
such as :command:`g++`),
since adding GNU Fortran to the GCC distribution
enables acceptance of GNU Fortran options
by all of the relevant drivers.

In some cases, options have positive and negative forms;
the negative form of :samp:`-ffoo` would be :samp:`-fno-foo`.
This manual documents only one of these two forms, whichever
one is not the default.