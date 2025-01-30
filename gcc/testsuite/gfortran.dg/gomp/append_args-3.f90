! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple -cpp" }

module my_omp_lib
 use iso_c_binding
 implicit none

 ! The following definitions are in omp_lib, which cannot be included
 ! in gcc/testsuite/
 integer, parameter :: omp_interop_kind = c_intptr_t
 integer, parameter :: omp_interop_fr_kind = c_int

 integer (omp_interop_kind), parameter :: omp_interop_none = 0_omp_interop_kind
 integer (omp_interop_fr_kind), parameter :: omp_ifr_cuda = 1
 integer (omp_interop_fr_kind), parameter :: omp_ifr_cuda_driver = 2
 integer (omp_interop_fr_kind), parameter :: omp_ifr_opencl = 3
 integer (omp_interop_fr_kind), parameter :: omp_ifr_sycl = 4
 integer (omp_interop_fr_kind), parameter :: omp_ifr_hip = 5
 integer (omp_interop_fr_kind), parameter :: omp_ifr_level_zero = 6
 integer (omp_interop_fr_kind), parameter :: omp_ifr_hsa = 7
end module my_omp_lib

module m
  use my_omp_lib
  implicit none (type, external)

  integer(omp_interop_kind) :: myobj_mod, myobj2_mod
  integer(omp_interop_kind), allocatable :: myobj_mod_alloc, myobj2_mod_alloc
contains

  subroutine vsub_no_arg (o_dummy, o_value)
    integer(omp_interop_kind) :: o_dummy
    integer(omp_interop_kind), value :: o_value
  end
  subroutine sub_no_arg ()
    !$omp declare variant (vsub_no_arg ) match(construct={dispatch}) append_args (interop(targetsync), interop( prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
  end

  integer(c_int) function vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value) bind(C)
    integer(c_int), value :: arg2_int
    character(len=1, kind=c_char) :: arg2_str(*)
    integer(omp_interop_kind) :: o2_dummy
    integer(omp_interop_kind), value :: o2_value
    vfun_cbind = arg2_int
  end
  integer(c_int) function fun_cbind(arg2_int, arg2_str) bind(C)
    !$omp declare variant(vfun_cbind)  , match(construct={dispatch}),append_args (interop(target), interop(target))
    integer(c_int), value :: arg2_int
    character(len=1, kind=c_char) :: arg2_str(*)
    fun_cbind = arg2_int
  end
end

subroutine test_sub_no_arg(obj_dummy_val, obj_dummy, obj_dummy_opt, obj_dummy_alloc, obj_dummy_alloc_opt, obj_dummy_ptr, obj_dummy_ptr_opt)
  use m
  implicit none (type, external)

  integer(omp_interop_kind), value :: obj_dummy_val
  integer(omp_interop_kind) :: obj_dummy
  integer(omp_interop_kind), optional :: obj_dummy_opt

  integer(omp_interop_kind), allocatable :: obj_dummy_alloc
  integer(omp_interop_kind), allocatable, optional :: obj_dummy_alloc_opt

  integer(omp_interop_kind), pointer :: obj_dummy_ptr
  integer(omp_interop_kind), pointer, optional :: obj_dummy_ptr_opt

  integer(omp_interop_kind), target :: obj_loc
  integer(omp_interop_kind), pointer :: obj_loc_ptr
  integer(omp_interop_kind), allocatable :: obj_loc_alloc

  obj_loc = omp_interop_none
  obj_loc_ptr => obj_loc

  !$omp dispatch device(10) interop(obj_dummy_val, obj_dummy)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(10\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj_dummy;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "obj_dummy_val\\.\[0-9\]+ = obj_dummy_val;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(&obj_dummy_val\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(11) interop(obj_dummy, obj_dummy_val)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(11\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(obj_dummy, obj_dummy_val\\);" 1 "gimple" } }

  !$omp dispatch device(12) interop(obj_dummy_opt, obj_dummy_alloc)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(12\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj_dummy_alloc;" 2 "gimple" } }
  ! The follow inline shows up 4x sub_no_arg and 4x vfun_cbind
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;" 8 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(obj_dummy_opt, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(13) interop(obj_dummy_alloc, obj_dummy_opt)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(13\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj_dummy_opt;" 1 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*obj_dummy_alloc;
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(D\\.\[0-9\]+, D\\.\[0-9\]+\\);" 3 "gimple" } }

  !$omp dispatch device(14) interop(obj_dummy_alloc_opt, obj_dummy_ptr)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(14\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj_dummy_ptr;" 2 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj_dummy_alloc_opt;" 2 "gimple" } }
  !              See above             vsub_no_arg \\(D\\.\[0-9\]+, D\\.\[0-9\]+\\);

  !$omp dispatch device(15) interop(obj_dummy_ptr, obj_dummy_alloc_opt)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(15\\);" 1 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*obj_dummy_alloc_opt;
  !              See above             D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;
  !              See above             D\\.\[0-9\]+ = \\*obj_dummy_ptr;
  !              See above             vsub_no_arg \\(D\\.\[0-9\]+, D\\.\[0-9\]+\\);

  !$omp dispatch device(16) interop(obj_dummy_ptr_opt, myobj_mod)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(16\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "myobj_mod\\.\[0-9\]+ = myobj_mod;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj_dummy_ptr_opt;" 2 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(D\\.\[0-9\]+, myobj_mod\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(17) interop(myobj_mod, obj_dummy_ptr_opt)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(17\\);" 1 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*obj_dummy_ptr_opt;
  !              See above             D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(&myobj_mod, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(18) interop(obj_loc, obj_loc_ptr)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(18\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj_loc_ptr;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(&obj_loc, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(19) interop(obj_loc_ptr, obj_loc)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(19\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "obj_loc\\.\[0-9\]+ = obj_loc;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(obj_loc_ptr, obj_loc\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(20) interop(obj_loc_alloc, myobj_mod_alloc)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(20\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "myobj_mod_alloc\\.\[0-9\]+ = myobj_mod_alloc;" 2 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*myobj_mod_alloc\\.\[0-9\]+;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(obj_loc_alloc, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(21) interop(myobj_mod_alloc, obj_loc_alloc)
    call sub_no_arg
  !  subroutine vsub_no_arg (o_dummy, o_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(21\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj_loc_alloc;" 1 "gimple" } }
  !              See above             myobj_mod_alloc\\.\[0-9\]+ = myobj_mod_alloc;
  ! { dg-final { scan-tree-dump-times "vsub_no_arg \\(myobj_mod_alloc\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }
end


integer(c_int) function test_fun_cbind (obj2_dummy_val, obj2_dummy, obj2_dummy_opt, obj2_dummy_alloc, obj2_dummy_alloc_opt, obj2_dummy_ptr, obj2_dummy_ptr_opt)
  use m
  implicit none (type, external)

  integer(omp_interop_kind), value :: obj2_dummy_val
  integer(omp_interop_kind) :: obj2_dummy
  integer(omp_interop_kind), optional :: obj2_dummy_opt

  integer(omp_interop_kind), allocatable :: obj2_dummy_alloc
  integer(omp_interop_kind), allocatable, optional :: obj2_dummy_alloc_opt

  integer(omp_interop_kind), pointer :: obj2_dummy_ptr
  integer(omp_interop_kind), pointer, optional :: obj2_dummy_ptr_opt

  integer(omp_interop_kind), target :: obj2_loc
  integer(omp_interop_kind), pointer :: obj2_loc_ptr
  integer(omp_interop_kind), allocatable :: obj2_loc_alloc

  integer :: i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41

  obj2_loc = omp_interop_none
  obj2_loc_ptr => obj2_loc

  !$omp dispatch device(30) interop(obj2_dummy, obj2_dummy_val)
    i30 = fun_cbind (300, "abc30" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(30\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i30 = vfun_cbind \\(300, &\"abc30\"\\\[1\\\]\{lb: 1 sz: 1\}, obj2_dummy, obj2_dummy_val\\);" 1 "gimple" } }

  !$omp dispatch device(31) interop(obj2_dummy_val, obj2_dummy)
    i31 = fun_cbind (301, "abc31" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(31\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj2_dummy;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "obj2_dummy_val\\.\[0-9\]+ = obj2_dummy_val;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i31 = vfun_cbind \\(301, &\"abc31\"\\\[1\\\]\{lb: 1 sz: 1\}, &obj2_dummy_val\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(32) interop(obj2_dummy_opt, obj2_dummy_alloc)
    i32 = fun_cbind (302, "abc32" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(32\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj2_dummy_alloc;" 2 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i32 = vfun_cbind \\(302, &\"abc32\"\\\[1\\\]\{lb: 1 sz: 1\}, obj2_dummy_opt, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(33) interop(obj2_dummy_alloc, obj2_dummy_opt)
    i33 = fun_cbind (303, "abc33" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(33\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj2_dummy_opt;" 1 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*obj2_dummy_alloc;
  ! { dg-final { scan-tree-dump-times "i33 = vfun_cbind \\(303, &\"abc33\"\\\[1\\\]\{lb: 1 sz: 1\}, D\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(34) interop(obj2_dummy_alloc_opt, obj2_dummy_ptr)
    i34 = fun_cbind (304, "abc34" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(34\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj2_dummy_ptr;" 2 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj2_dummy_alloc_opt;" 2 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i34 = vfun_cbind \\(304, &\"abc34\"\\\[1\\\]\{lb: 1 sz: 1\}, D\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(35) interop(obj2_dummy_ptr, obj2_dummy_alloc_opt)
    i35 = fun_cbind (305, "abc35" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(35\\);" 1 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*obj2_dummy_alloc_opt;
  !              See above             D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;" 1 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*obj2_dummy_ptr;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i35 = vfun_cbind \\(305, &\"abc35\"\\\[1\\\]\{lb: 1 sz: 1\}, D\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(36) interop(obj2_dummy_ptr_opt, myobj2_mod)
    i36 = fun_cbind (306, "abc36" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(36\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "myobj2_mod\\.\[0-9\]+ = myobj2_mod;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj2_dummy_ptr_opt;" 2 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i36 = vfun_cbind \\(306, &\"abc36\"\\\[1\\\]\{lb: 1 sz: 1\}, D\\.\[0-9\]+, myobj2_mod\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(37) interop(myobj2_mod, obj2_dummy_ptr_opt)
    i37 = fun_cbind (307, "abc37" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(37\\);" 1 "gimple" } }
  !              See above             D\\.\[0-9\]+ = \\*obj2_dummy_ptr_opt;
  !              See above             D\\.\[0-9\]+ = \\*D\\.\[0-9\]+;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i37 = vfun_cbind \\(307, &\"abc37\"\\\[1\\\]\{lb: 1 sz: 1\}, &myobj2_mod, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(38) interop(obj2_loc, obj2_loc_ptr)
    i38 = fun_cbind (308, "abc38" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(38\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj2_loc_ptr;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i38 = vfun_cbind \\(308, &\"abc38\"\\\[1\\\]\{lb: 1 sz: 1\}, &obj2_loc, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(39) interop(obj2_loc_ptr, obj2_loc)
    i39 = fun_cbind (309, "abc39" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(39\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "obj2_loc\\.\[0-9\]+ = obj2_loc;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i39 = vfun_cbind \\(309, &\"abc39\"\\\[1\\\]\{lb: 1 sz: 1\}, obj2_loc_ptr, obj2_loc\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(40) interop(obj2_loc_alloc, myobj2_mod_alloc)
    i40 = fun_cbind (400, "abc40" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(40\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "myobj2_mod_alloc\\.\[0-9\]+ = myobj2_mod_alloc;" 2 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*myobj2_mod_alloc\\.\[0-9\]+;" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "i40 = vfun_cbind \\(400, &\"abc40\"\\\[1\\\]\{lb: 1 sz: 1\}, obj2_loc_alloc, D\\.\[0-9\]+\\);" 1 "gimple" } }

  !$omp dispatch device(41) interop(myobj2_mod_alloc, obj2_loc_alloc)
    i41 = fun_cbind (401, "abc41" // c_null_char)
  !  vfun_cbind(arg2_int, arg2_str, o2_dummy, o2_value)
  ! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(41\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = \\*obj2_loc_alloc;" 1 "gimple" } }
  !              See above             myobj2_mod_alloc\\.\[0-9\]+ = myobj2_mod_alloc;
  ! { dg-final { scan-tree-dump-times "i41 = vfun_cbind \\(401, &\"abc41\"\\\[1\\\]\{lb: 1 sz: 1\}, myobj2_mod_alloc\\.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimple" } }

  test_fun_cbind = i30 + i31 + i32 + i33 + i34 + i35 + i36 + i37 + i38 + i39 + i40 + i41
end

! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_default_device \\(\\);" 24 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_omp_set_default_device \\(D\\.\[0-9\]+\\);" 24 "gimple" } }
