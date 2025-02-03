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

  integer function vifun (str, int_opt, alloc_str, o_dummy, o_value)
    character(len=*) :: str
    integer, optional, value :: int_opt
    character(len=:), allocatable :: alloc_str
    integer(omp_interop_kind) :: o_dummy
    integer(omp_interop_kind), value :: o_value
    vifun = 0
  end
  integer function ifun (str, int_opt, alloc_str)
    character(len=*) :: str
    integer, optional, value :: int_opt
    character(len=:), allocatable :: alloc_str
    !$omp declare variant (vifun ) match(construct={dispatch}) append_args (interop(targetsync), interop( prefer_type ( {fr("cuda"), attr("ompx_xx")}, {attr("ompx_yy")} )))
    ifun = 0
  end

  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
    character(len=*) :: str
    integer, value :: int_var
    integer, optional, value :: int_opt
    character(len=:), allocatable :: alloc_str
    integer(omp_interop_kind) :: o2_dummy
    integer(omp_interop_kind), value :: o2_value
    vfun = ""
  end
  character(len=len(str)) function fun (str, int_var, int_opt, alloc_str)
    !$omp declare variant(vfun), match(construct={dispatch}),append_args (interop(target), interop(target))
    character(len=*) :: str
    integer, value :: int_var
    integer, optional, value :: int_opt
    character(len=:), allocatable :: alloc_str
    fun = ""
  end
end

integer function test_ifun(obj_dummy_val, obj_dummy, obj_dummy_opt, obj_dummy_alloc, obj_dummy_alloc_opt, obj_dummy_ptr, obj_dummy_ptr_opt)
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

  character(len=:), allocatable :: str_alloc
  integer :: i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21

  obj_loc = omp_interop_none
  obj_loc_ptr => obj_loc

  !$omp dispatch device(10) interop(obj_dummy_val, obj_dummy)
    i10 = ifun ("abc10", i10, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i10 = vifun \\(&\"abc10\"\\\[1\\\]\{lb: 1 sz: 1\}, i10, &str_alloc, &obj_dummy_val\\.\[0-9\]+, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(11) interop(obj_dummy, obj_dummy_val)
    i11 = ifun ("abc11", i11, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i11 = vifun \\(&\"abc11\"\\\[1\\\]\{lb: 1 sz: 1\}, i11, &str_alloc, obj_dummy, obj_dummy_val, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(12) interop(obj_dummy_opt, obj_dummy_alloc)
    i12 = ifun ("abc12", i12, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i12 = vifun \\(&\"abc12\"\\\[1\\\]\{lb: 1 sz: 1\}, i12, &str_alloc, obj_dummy_opt, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(13) interop(obj_dummy_alloc, obj_dummy_opt)
    i13 = ifun ("abc13", i13, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i13 = vifun \\(&\"abc13\"\\\[1\\\]\{lb: 1 sz: 1\}, i13, &str_alloc, D\.\[0-9\]+, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(14) interop(obj_dummy_alloc_opt, obj_dummy_ptr)
    i14 = ifun ("abc14", i14, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i14 = vifun \\(&\"abc14\"\\\[1\\\]\{lb: 1 sz: 1\}, i14, &str_alloc, D\.\[0-9\]+, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(15) interop(obj_dummy_ptr, obj_dummy_alloc_opt)
    i15 = ifun ("abc15", i15, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i15 = vifun \\(&\"abc15\"\\\[1\\\]\{lb: 1 sz: 1\}, i15, &str_alloc, D\.\[0-9\]+, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(16) interop(obj_dummy_ptr_opt, myobj_mod)
    i16 = ifun ("abc16", i16, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i16 = vifun \\(&\"abc16\"\\\[1\\\]\{lb: 1 sz: 1\}, i16, &str_alloc, D\.\[0-9\]+, myobj_mod\\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(17) interop(myobj_mod, obj_dummy_ptr_opt)
    i17 = ifun ("abc17", i17, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i17 = vifun \\(&\"abc17\"\\\[1\\\]\{lb: 1 sz: 1\}, i17, &str_alloc, &myobj_mod, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(18) interop(obj_loc, obj_loc_ptr)
    i18 = ifun ("abc18", i18, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i18 = vifun \\(&\"abc18\"\\\[1\\\]\{lb: 1 sz: 1\}, i18, &str_alloc, &obj_loc, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(19) interop(obj_loc_ptr, obj_loc)
    i19 = ifun ("abc19", i19, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i19 = vifun \\(&\"abc19\"\\\[1\\\]\{lb: 1 sz: 1\}, i19, &str_alloc, obj_loc_ptr, obj_loc\\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(20) interop(obj_loc_alloc, myobj_mod_alloc)
    i20 = ifun ("abc20", i20, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i20 = vifun \\(&\"abc20\"\\\[1\\\]\{lb: 1 sz: 1\}, i20, &str_alloc, obj_loc_alloc, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  !$omp dispatch device(21) interop(myobj_mod_alloc, obj_loc_alloc)
    i21 = ifun ("abc21", i21, str_alloc)
  !  integer function ifun (str, int_opt, alloc_str, o_dummy, o_value)
  !  -> int vifun (str, int_opt, alloc_str, o_dummy, o_value, .int_opt, _str, _alloc_str)
  ! { dg-final { scan-tree-dump-times "i21 = vifun \\(&\"abc21\"\\\[1\\\]\{lb: 1 sz: 1\}, i21, &str_alloc, myobj_mod_alloc\\.\[0-9\]+, D\.\[0-9\]+, 1, 5, &.str_alloc\\);" 1 "gimple" } }

  test_ifun = i10 + i11 + i12 + i13 + i14 + i15 + i16 + i17 + i18 + i19 + i20 + i21
end


integer(c_int) function test_fun (obj2_dummy_val, obj2_dummy, obj2_dummy_opt, obj2_dummy_alloc, obj2_dummy_alloc_opt, obj2_dummy_ptr, obj2_dummy_ptr_opt)
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

  character(len=:), allocatable :: str_alloc
  character(1) :: res_str
  integer :: i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41
  i30 = 0; i31 = 0; i32 = 0; i33 = 0; i34 = 0; i35 = 0; i36 = 0; i37 = 0; i38 = 0; i39 = 0; i40 = 0; i41 = 0

  obj2_loc = omp_interop_none
  obj2_loc_ptr => obj2_loc

  !$omp dispatch device(30) interop(obj2_dummy, obj2_dummy_val)
    res_str = fun ("klm30", 300, i30, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(31) interop(obj2_dummy_val, obj2_dummy)
    res_str = fun ("klm31", 301, i31, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(32) interop(obj2_dummy_opt, obj2_dummy_alloc)
    res_str = fun ("klm32", 302, i32, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(33) interop(obj2_dummy_alloc, obj2_dummy_opt)
    res_str = fun ("klm33", 303, i33, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(34) interop(obj2_dummy_alloc_opt, obj2_dummy_ptr)
    res_str = fun ("klm34", 304, i34, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(35) interop(obj2_dummy_ptr, obj2_dummy_alloc_opt)
    res_str = fun ("klm35", 305, i35, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(36) interop(obj2_dummy_ptr_opt, myobj2_mod)
    res_str = fun ("klm36", 306, i36, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(37) interop(myobj2_mod, obj2_dummy_ptr_opt)
    res_str = fun ("klm37", 307, i37, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(38) interop(obj2_loc, obj2_loc_ptr)
    res_str = fun ("klm30", 308, i38, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(39) interop(obj2_loc_ptr, obj2_loc)
    res_str = fun ("klm39", 309, i39, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  !$omp dispatch device(40) interop(obj2_loc_alloc, myobj2_mod_alloc)
    res_str = fun ("klm40", 400, i40, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)

  !$omp dispatch device(41) interop(myobj2_mod_alloc, obj2_loc_alloc)
    res_str = fun ("klm41", 401, i41, str_alloc)
  !  character(len=len(str)) function vfun (str, int_var, int_opt, alloc_str, o2_dummy, o2_value)
  !  -> void vfun (__result, .__result, str, int_var, int_opt, alloc_str, o2_dummy, o2_value, .int_opt, _str, _alloc_str)

  test_fun = i30 + i31 + i32 + i33 + i34 + i35 + i36 + i37 + i38 + i39 + i40 + i41
end
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 300, D\\.\[0-9\]+, D\\.\[0-9\]+, obj2_dummy, obj2_dummy_val, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 301, D\\.\[0-9\]+, D\\.\[0-9\]+, &obj2_dummy_val\\.\[0-9\]+, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 302, D\\.\[0-9\]+, D\\.\[0-9\]+, obj2_dummy_opt, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 303, D\\.\[0-9\]+, D\\.\[0-9\]+, D\\.\[0-9\]+, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 304, D\\.\[0-9\]+, D\\.\[0-9\]+, D\\.\[0-9\]+, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 305, D\\.\[0-9\]+, D\\.\[0-9\]+, D\\.\[0-9\]+, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 306, D\\.\[0-9\]+, D\\.\[0-9\]+, D\\.\[0-9\]+, myobj2_mod\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 307, D\\.\[0-9\]+, D\\.\[0-9\]+, &myobj2_mod, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 308, D\\.\[0-9\]+, D\\.\[0-9\]+, &obj2_loc, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 309, D\\.\[0-9\]+, D\\.\[0-9\]+, obj2_loc_ptr, obj2_loc\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 400, D\\.\[0-9\]+, D\\.\[0-9\]+, obj2_loc_alloc, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "vfun \\(&str\\.\[0-9\]+, 5, D\\.\[0-9\]+, 401, D\\.\[0-9\]+, D\\.\[0-9\]+, myobj2_mod_alloc\\.\[0-9\]+, D\\.\[0-9\]+, 1, 5, &.str_alloc\\.\[0-9\]+\\);" 1 "gimple" } }
