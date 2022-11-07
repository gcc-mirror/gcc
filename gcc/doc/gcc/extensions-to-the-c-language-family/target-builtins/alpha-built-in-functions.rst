..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _alpha-built-in-functions:

Alpha Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the Alpha family of
processors, depending on the command-line switches used.

The following built-in functions are always available.  They
all generate the machine instruction that is part of the name.

.. code-block:: c++

  long __builtin_alpha_implver (void);
  long __builtin_alpha_rpcc (void);
  long __builtin_alpha_amask (long);
  long __builtin_alpha_cmpbge (long, long);
  long __builtin_alpha_extbl (long, long);
  long __builtin_alpha_extwl (long, long);
  long __builtin_alpha_extll (long, long);
  long __builtin_alpha_extql (long, long);
  long __builtin_alpha_extwh (long, long);
  long __builtin_alpha_extlh (long, long);
  long __builtin_alpha_extqh (long, long);
  long __builtin_alpha_insbl (long, long);
  long __builtin_alpha_inswl (long, long);
  long __builtin_alpha_insll (long, long);
  long __builtin_alpha_insql (long, long);
  long __builtin_alpha_inswh (long, long);
  long __builtin_alpha_inslh (long, long);
  long __builtin_alpha_insqh (long, long);
  long __builtin_alpha_mskbl (long, long);
  long __builtin_alpha_mskwl (long, long);
  long __builtin_alpha_mskll (long, long);
  long __builtin_alpha_mskql (long, long);
  long __builtin_alpha_mskwh (long, long);
  long __builtin_alpha_msklh (long, long);
  long __builtin_alpha_mskqh (long, long);
  long __builtin_alpha_umulh (long, long);
  long __builtin_alpha_zap (long, long);
  long __builtin_alpha_zapnot (long, long);

The following built-in functions are always with :option:`-mmax`
or :option:`-mcpu=cpu` where :samp:`{cpu}` is ``pca56`` or
later.  They all generate the machine instruction that is part
of the name.

.. code-block:: c++

  long __builtin_alpha_pklb (long);
  long __builtin_alpha_pkwb (long);
  long __builtin_alpha_unpkbl (long);
  long __builtin_alpha_unpkbw (long);
  long __builtin_alpha_minub8 (long, long);
  long __builtin_alpha_minsb8 (long, long);
  long __builtin_alpha_minuw4 (long, long);
  long __builtin_alpha_minsw4 (long, long);
  long __builtin_alpha_maxub8 (long, long);
  long __builtin_alpha_maxsb8 (long, long);
  long __builtin_alpha_maxuw4 (long, long);
  long __builtin_alpha_maxsw4 (long, long);
  long __builtin_alpha_perr (long, long);

The following built-in functions are always with :option:`-mcix`
or :option:`-mcpu=cpu` where :samp:`{cpu}` is ``ev67`` or
later.  They all generate the machine instruction that is part
of the name.

.. code-block:: c++

  long __builtin_alpha_cttz (long);
  long __builtin_alpha_ctlz (long);
  long __builtin_alpha_ctpop (long);

The following built-in functions are available on systems that use the OSF/1
PALcode.  Normally they invoke the ``rduniq`` and ``wruniq``
PAL calls, but when invoked with :option:`-mtls-kernel`, they invoke
``rdval`` and ``wrval``.

.. code-block:: c++

  void *__builtin_thread_pointer (void);
  void __builtin_set_thread_pointer (void *);