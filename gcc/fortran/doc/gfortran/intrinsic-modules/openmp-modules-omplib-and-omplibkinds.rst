..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _openmp-modules-omp_lib-and-omp_lib_kinds:

OpenMP Modules OMP_LIB and OMP_LIB_KINDS
****************************************

The OpenMP Fortran runtime library routines are provided both in
a form of two Fortran modules, named ``OMP_LIB`` and
``OMP_LIB_KINDS``, and in a form of a Fortran ``include`` file named
:samp:`omp_lib.h`. The procedures provided by ``OMP_LIB`` can be found
in the :ref:`libgomp:top` manual,
the named constants defined in the modules are listed
below.

For details refer to the actual
`OpenMP Application Program Interface v4.5 <https://www.openmp.org/wp-content/uploads/openmp-4.5.pdf>`_ and
`OpenMP Application Program Interface v5.0 <https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-5.0.pdf>`_.

``OMP_LIB_KINDS`` provides the following scalar default-integer
named constants:

.. code-block::

  omp_allocator_handle_kind
  omp_alloctrait_key_kind
  omp_alloctrait_val_kind
  omp_depend_kind
  omp_lock_kind
  omp_lock_hint_kind
  omp_nest_lock_kind
  omp_pause_resource_kind
  omp_memspace_handle_kind
  omp_proc_bind_kind
  omp_sched_kind
  omp_sync_hint_kind

``OMP_LIB`` provides the scalar default-integer
named constant ``openmp_version`` with a value of the form
:samp:`{yyyymm}`, where ``yyyy`` is the year and :samp:`{mm}` the month
of the OpenMP version; for OpenMP v4.5 the value is ``201511``.

The following derived type:

.. code-block::

  omp_alloctrait

The following scalar integer named constants of the
kind ``omp_sched_kind`` :

.. code-block::

  omp_sched_static
  omp_sched_dynamic
  omp_sched_guided
  omp_sched_auto

And the following scalar integer named constants of the
kind ``omp_proc_bind_kind`` :

.. code-block::

  omp_proc_bind_false
  omp_proc_bind_true
  omp_proc_bind_primary
  omp_proc_bind_master
  omp_proc_bind_close
  omp_proc_bind_spread

The following scalar integer named constants are of the
kind ``omp_lock_hint_kind`` :

.. code-block::

  omp_lock_hint_none
  omp_lock_hint_uncontended
  omp_lock_hint_contended
  omp_lock_hint_nonspeculative
  omp_lock_hint_speculative
  omp_sync_hint_none
  omp_sync_hint_uncontended
  omp_sync_hint_contended
  omp_sync_hint_nonspeculative
  omp_sync_hint_speculative

And the following two scalar integer named constants are of the
kind ``omp_pause_resource_kind`` :

.. code-block::

  omp_pause_soft
  omp_pause_hard

The following scalar integer named constants are of the kind
``omp_alloctrait_key_kind`` :

.. code-block::

  omp_atk_sync_hint
  omp_atk_alignment
  omp_atk_access
  omp_atk_pool_size
  omp_atk_fallback
  omp_atk_fb_data
  omp_atk_pinned
  omp_atk_partition

The following scalar integer named constants are of the kind
``omp_alloctrait_val_kind`` :

.. code-block::

  omp_atv_default
  omp_atv_false
  omp_atv_true
  omp_atv_contended
  omp_atv_uncontended
  omp_atv_serialized
  omp_atv_sequential
  omp_atv_private
  omp_atv_all
  omp_atv_thread
  omp_atv_pteam
  omp_atv_cgroup
  omp_atv_default_mem_fb
  omp_atv_null_fb
  omp_atv_abort_fb
  omp_atv_allocator_fb
  omp_atv_environment
  omp_atv_nearest
  omp_atv_blocked

The following scalar integer named constants are of the kind
``omp_allocator_handle_kind`` :

.. code-block::

  omp_null_allocator
  omp_default_mem_alloc
  omp_large_cap_mem_alloc
  omp_const_mem_alloc
  omp_high_bw_mem_alloc
  omp_low_lat_mem_alloc
  omp_cgroup_mem_alloc
  omp_pteam_mem_alloc
  omp_thread_mem_alloc

The following scalar integer named constants are of the kind
``omp_memspace_handle_kind`` :

.. code-block::

  omp_default_mem_space
  omp_large_cap_mem_space
  omp_const_mem_space
  omp_high_bw_mem_space
  omp_low_lat_mem_space