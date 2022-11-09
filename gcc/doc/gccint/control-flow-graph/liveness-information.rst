..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Liveness representation

.. _liveness-information:

Liveness information
********************

Liveness information is useful to determine whether some register is
'live' at given point of program, i.e. that it contains a value that
may be used at a later point in the program.  This information is
used, for instance, during register allocation, as the pseudo
registers only need to be assigned to a unique hard register or to a
stack slot if they are live.  The hard registers and stack slots may
be freely reused for other values when a register is dead.

Liveness information is available in the back end starting with
``pass_df_initialize`` and ending with ``pass_df_finish``.  Three
flavors of live analysis are available: With ``LR``, it is possible
to determine at any point ``P`` in the function if the register may be
used on some path from ``P`` to the end of the function.  With
``UR``, it is possible to determine if there is a path from the
beginning of the function to ``P`` that defines the variable.
``LIVE`` is the intersection of the ``LR`` and ``UR`` and a
variable is live at ``P`` if there is both an assignment that reaches
it from the beginning of the function and a use that can be reached on
some path from ``P`` to the end of the function.

In general ``LIVE`` is the most useful of the three.  The macros
``DF_[LR,UR,LIVE]_[IN,OUT]`` can be used to access this information.
The macros take a basic block number and return a bitmap that is indexed
by the register number.  This information is only guaranteed to be up to
date after calls are made to ``df_analyze``.  See the file
``df-core.cc`` for details on using the dataflow.

.. index:: REG_DEAD, REG_UNUSED

The liveness information is stored partly in the RTL instruction stream
and partly in the flow graph.  Local information is stored in the
instruction stream: Each instruction may contain ``REG_DEAD`` notes
representing that the value of a given register is no longer needed, or
``REG_UNUSED`` notes representing that the value computed by the
instruction is never used.  The second is useful for instructions
computing multiple values at once.
