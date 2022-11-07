..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _amd-gcn-function-attributes:

AMD GCN Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the AMD GCN back end:

.. index:: amdgpu_hsa_kernel function attribute, AMD GCN

.. amd-gcn-fn-attr:: amdgpu_hsa_kernel

  This attribute indicates that the corresponding function should be compiled as
  a kernel function, that is an entry point that can be invoked from the host
  via the HSA runtime library.  By default functions are only callable only from
  other GCN functions.

  This attribute is implicitly applied to any function named ``main``, using
  default parameters.

  Kernel functions may return an integer value, which will be written to a
  conventional place within the HSA "kernargs" region.

  The attribute parameters configure what values are passed into the kernel
  function by the GPU drivers, via the initial register state.  Some values are
  used by the compiler, and therefore forced on.  Enabling other options may
  break assumptions in the compiler and/or run-time libraries.

  ``private_segment_buffer``
    Set ``enable_sgpr_private_segment_buffer`` flag.  Always on (required to
    locate the stack).

  ``dispatch_ptr``
    Set ``enable_sgpr_dispatch_ptr`` flag.  Always on (required to locate the
    launch dimensions).

  ``queue_ptr``
    Set ``enable_sgpr_queue_ptr`` flag.  Always on (required to convert address
    spaces).

  ``kernarg_segment_ptr``
    Set ``enable_sgpr_kernarg_segment_ptr`` flag.  Always on (required to
    locate the kernel arguments, "kernargs").

  ``dispatch_id``
    Set ``enable_sgpr_dispatch_id`` flag.

  ``flat_scratch_init``
    Set ``enable_sgpr_flat_scratch_init`` flag.

  ``private_segment_size``
    Set ``enable_sgpr_private_segment_size`` flag.

  ``grid_workgroup_count_X``
    Set ``enable_sgpr_grid_workgroup_count_x`` flag.  Always on (required to
    use OpenACC/OpenMP).

  ``grid_workgroup_count_Y``
    Set ``enable_sgpr_grid_workgroup_count_y`` flag.

  ``grid_workgroup_count_Z``
    Set ``enable_sgpr_grid_workgroup_count_z`` flag.

  ``workgroup_id_X``
    Set ``enable_sgpr_workgroup_id_x`` flag.

  ``workgroup_id_Y``
    Set ``enable_sgpr_workgroup_id_y`` flag.

  ``workgroup_id_Z``
    Set ``enable_sgpr_workgroup_id_z`` flag.

  ``workgroup_info``
    Set ``enable_sgpr_workgroup_info`` flag.

  ``private_segment_wave_offset``
    Set ``enable_sgpr_private_segment_wave_byte_offset`` flag.  Always on
    (required to locate the stack).

  ``work_item_id_X``
    Set ``enable_vgpr_workitem_id`` parameter.  Always on (can't be disabled).

  ``work_item_id_Y``
    Set ``enable_vgpr_workitem_id`` parameter.  Always on (required to enable
    vectorization.)

  ``work_item_id_Z``
    Set ``enable_vgpr_workitem_id`` parameter.  Always on (required to use
    OpenACC/OpenMP).