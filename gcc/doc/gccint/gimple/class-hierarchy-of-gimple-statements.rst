..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE class hierarchy

.. _class-hierarchy-of-gimple-statements:

Class hierarchy of GIMPLE statements
************************************

The following diagram shows the C++ inheritance hierarchy of statement
kinds, along with their relationships to ``GSS_`` values (layouts) and
``GIMPLE_`` values (codes):

.. code-block:: c++

     gimple
       |    layout: GSS_BASE
       |    used for 4 codes: GIMPLE_ERROR_MARK
       |                      GIMPLE_NOP
       |                      GIMPLE_OMP_SECTIONS_SWITCH
       |                      GIMPLE_PREDICT
       |
       + gimple_statement_with_ops_base
       |   |    (no GSS layout)
       |   |
       |   + gimple_statement_with_ops
       |   |   |    layout: GSS_WITH_OPS
       |   |   |
       |   |   + gcond
       |   |   |     code: GIMPLE_COND
       |   |   |
       |   |   + gdebug
       |   |   |     code: GIMPLE_DEBUG
       |   |   |
       |   |   + ggoto
       |   |   |     code: GIMPLE_GOTO
       |   |   |
       |   |   + glabel
       |   |   |     code: GIMPLE_LABEL
       |   |   |
       |   |   + gswitch
       |   |         code: GIMPLE_SWITCH
       |   |
       |   + gimple_statement_with_memory_ops_base
       |       |    layout: GSS_WITH_MEM_OPS_BASE
       |       |
       |       + gimple_statement_with_memory_ops
       |       |   |    layout: GSS_WITH_MEM_OPS
       |       |   |
       |       |   + gassign
       |       |   |    code GIMPLE_ASSIGN
       |       |   |
       |       |   + greturn
       |       |        code GIMPLE_RETURN
       |       |
       |       + gcall
       |       |        layout: GSS_CALL, code: GIMPLE_CALL
       |       |
       |       + gasm
       |       |        layout: GSS_ASM, code: GIMPLE_ASM
       |       |
       |       + gtransaction
       |                layout: GSS_TRANSACTION, code: GIMPLE_TRANSACTION
       |
       + gimple_statement_omp
       |   |    layout: GSS_OMP.  Used for code GIMPLE_OMP_SECTION
       |   |
       |   + gomp_critical
       |   |        layout: GSS_OMP_CRITICAL, code: GIMPLE_OMP_CRITICAL
       |   |
       |   + gomp_for
       |   |        layout: GSS_OMP_FOR, code: GIMPLE_OMP_FOR
       |   |
       |   + gomp_parallel_layout
       |   |   |    layout: GSS_OMP_PARALLEL_LAYOUT
       |   |   |
       |   |   + gimple_statement_omp_taskreg
       |   |   |   |
       |   |   |   + gomp_parallel
       |   |   |   |        code: GIMPLE_OMP_PARALLEL
       |   |   |   |
       |   |   |   + gomp_task
       |   |   |            code: GIMPLE_OMP_TASK
       |   |   |
       |   |   + gimple_statement_omp_target
       |   |            code: GIMPLE_OMP_TARGET
       |   |
       |   + gomp_sections
       |   |        layout: GSS_OMP_SECTIONS, code: GIMPLE_OMP_SECTIONS
       |   |
       |   + gimple_statement_omp_single_layout
       |       |    layout: GSS_OMP_SINGLE_LAYOUT
       |       |
       |       + gomp_single
       |       |        code: GIMPLE_OMP_SINGLE
       |       |
       |       + gomp_teams
       |                code: GIMPLE_OMP_TEAMS
       |
       + gbind
       |        layout: GSS_BIND, code: GIMPLE_BIND
       |
       + gcatch
       |        layout: GSS_CATCH, code: GIMPLE_CATCH
       |
       + geh_filter
       |        layout: GSS_EH_FILTER, code: GIMPLE_EH_FILTER
       |
       + geh_else
       |        layout: GSS_EH_ELSE, code: GIMPLE_EH_ELSE
       |
       + geh_mnt
       |        layout: GSS_EH_MNT, code: GIMPLE_EH_MUST_NOT_THROW
       |
       + gphi
       |        layout: GSS_PHI, code: GIMPLE_PHI
       |
       + gimple_statement_eh_ctrl
       |   |    layout: GSS_EH_CTRL
       |   |
       |   + gresx
       |   |        code: GIMPLE_RESX
       |   |
       |   + geh_dispatch
       |            code: GIMPLE_EH_DISPATCH
       |
       + gtry
       |        layout: GSS_TRY, code: GIMPLE_TRY
       |
       + gimple_statement_wce
       |        layout: GSS_WCE, code: GIMPLE_WITH_CLEANUP_EXPR
       |
       + gomp_continue
       |        layout: GSS_OMP_CONTINUE, code: GIMPLE_OMP_CONTINUE
       |
       + gomp_atomic_load
       |        layout: GSS_OMP_ATOMIC_LOAD, code: GIMPLE_OMP_ATOMIC_LOAD
       |
       + gimple_statement_omp_atomic_store_layout
           |    layout: GSS_OMP_ATOMIC_STORE_LAYOUT,
           |    code: GIMPLE_OMP_ATOMIC_STORE
           |
           + gomp_atomic_store
           |        code: GIMPLE_OMP_ATOMIC_STORE
           |
           + gomp_return
                    code: GIMPLE_OMP_RETURN