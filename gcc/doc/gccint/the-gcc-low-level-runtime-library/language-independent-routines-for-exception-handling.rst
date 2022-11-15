..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _exception-handling-routines:

Language-independent routines for exception handling
****************************************************

document me!

.. code-block:: c++

    _Unwind_DeleteException
    _Unwind_Find_FDE
    _Unwind_ForcedUnwind
    _Unwind_GetGR
    _Unwind_GetIP
    _Unwind_GetLanguageSpecificData
    _Unwind_GetRegionStart
    _Unwind_GetTextRelBase
    _Unwind_GetDataRelBase
    _Unwind_RaiseException
    _Unwind_Resume
    _Unwind_SetGR
    _Unwind_SetIP
    _Unwind_FindEnclosingFunction
    _Unwind_SjLj_Register
    _Unwind_SjLj_Unregister
    _Unwind_SjLj_RaiseException
    _Unwind_SjLj_ForcedUnwind
    _Unwind_SjLj_Resume
    __deregister_frame
    __deregister_frame_info
    __deregister_frame_info_bases
    __register_frame
    __register_frame_info
    __register_frame_info_bases
    __register_frame_info_table
    __register_frame_info_table_bases
    __register_frame_table
