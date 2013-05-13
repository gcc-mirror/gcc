! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/39505
! 
! Test NO_ARG_CHECK
! Copied from assumed_type_2.f90
!

module mod
  use iso_c_binding, only: c_loc, c_ptr, c_bool
  implicit none
  interface my_c_loc
    function my_c_loc1(x) bind(C)
      import c_ptr
!GCC$ attributes NO_ARG_CHECK :: x
      type(*) :: x
      type(c_ptr) :: my_c_loc1
    end function
  end interface my_c_loc
contains
  subroutine sub_scalar (arg1, presnt)
     integer(8), target, optional :: arg1
     logical :: presnt
     type(c_ptr) :: cpt
!GCC$ attributes NO_ARG_CHECK :: arg1
     if (presnt .neqv. present (arg1)) call abort ()
     cpt = c_loc (arg1)
  end subroutine sub_scalar

  subroutine sub_array_assumed (arg3)
!GCC$ attributes NO_ARG_CHECK :: arg3
     logical(1), target :: arg3(*)
     type(c_ptr) :: cpt
     cpt = c_loc (arg3)
  end subroutine sub_array_assumed
end module

use mod
use iso_c_binding, only: c_int, c_null_ptr
implicit none
type t1
  integer :: a
end type t1
type :: t2
  sequence
  integer :: b
end type t2
type, bind(C) :: t3
  integer(c_int) :: c
end type t3

integer            :: scalar_int
real, allocatable  :: scalar_real_alloc
character, pointer :: scalar_char_ptr

integer            :: array_int(3)
real, allocatable  :: array_real_alloc(:,:)
character, pointer :: array_char_ptr(:,:)

type(t1)              :: scalar_t1
type(t2), allocatable :: scalar_t2_alloc
type(t3), pointer     :: scalar_t3_ptr

type(t1)              :: array_t1(4)
type(t2), allocatable :: array_t2_alloc(:,:)
type(t3), pointer     :: array_t3_ptr(:,:)

class(t1), allocatable :: scalar_class_t1_alloc
class(t1), pointer     :: scalar_class_t1_ptr

class(t1), allocatable :: array_class_t1_alloc(:,:)
class(t1), pointer     :: array_class_t1_ptr(:,:)

scalar_char_ptr => null()
scalar_t3_ptr => null()

call sub_scalar (presnt=.false.)
call sub_scalar (scalar_real_alloc, .false.)
call sub_scalar (scalar_char_ptr, .false.)
call sub_scalar (null (), .false.)
call sub_scalar (scalar_t2_alloc, .false.)
call sub_scalar (scalar_t3_ptr, .false.)

allocate (scalar_real_alloc, scalar_char_ptr, scalar_t3_ptr)
allocate (scalar_class_t1_alloc, scalar_class_t1_ptr, scalar_t2_alloc)
allocate (array_real_alloc(3:5,2:4), array_char_ptr(-2:2,2))
allocate (array_t2_alloc(3:5,2:4), array_t3_ptr(-2:2,2))
allocate (array_class_t1_alloc(3,3), array_class_t1_ptr(4,4))

call sub_scalar (scalar_int, .true.)
call sub_scalar (scalar_real_alloc, .true.)
call sub_scalar (scalar_char_ptr, .true.)
call sub_scalar (array_int(2), .true.)
call sub_scalar (array_real_alloc(3,2), .true.)
call sub_scalar (array_char_ptr(0,1), .true.)
call sub_scalar (scalar_t1, .true.)
call sub_scalar (scalar_t2_alloc, .true.)
call sub_scalar (scalar_t3_ptr, .true.)
call sub_scalar (array_t1(2), .true.)
call sub_scalar (array_t2_alloc(3,2), .true.)
call sub_scalar (array_t3_ptr(0,1), .true.)
call sub_scalar (array_class_t1_alloc(2,1), .true.)
call sub_scalar (array_class_t1_ptr(3,3), .true.)

call sub_array_assumed (array_int)
call sub_array_assumed (array_real_alloc)
call sub_array_assumed (array_char_ptr)
call sub_array_assumed (array_t1)
call sub_array_assumed (array_t2_alloc)
call sub_array_assumed (array_t3_ptr)
call sub_array_assumed (array_class_t1_alloc)
call sub_array_assumed (array_class_t1_ptr)

deallocate (scalar_char_ptr, scalar_class_t1_ptr, array_char_ptr)
deallocate (array_class_t1_ptr, array_t3_ptr)
contains
  subroutine sub(x)
    integer :: x(:)
    call sub_array_assumed (x)
  end subroutine sub
end

! { dg-final { scan-tree-dump-times "sub_scalar .0B,"  2 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .scalar_real_alloc," 2 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .scalar_char_ptr," 2 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .scalar_t2_alloc," 2 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .scalar_t3_ptr" 2 "original" } }

! { dg-final { scan-tree-dump-times "sub_scalar .&scalar_int," 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .&scalar_t1," 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .&array_int.1.," 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .&scalar_t1," 1 "original" } }

! { dg-final { scan-tree-dump-times "sub_scalar .&\\(.\\(real.kind=4..0:. . restrict\\) array_real_alloc.data" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .&\\(.\\(character.kind=1..0:..1:1. .\\) array_char_ptr.data" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .&\\(.\\(struct t2.0:. . restrict\\) array_t2_alloc.data" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .&\\(.\\(struct t3.0:. .\\) array_t3_ptr.data" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .\\(struct t1 .\\) array_class_t1_alloc._data.data" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_scalar .\\(struct t1 .\\) array_class_t1_ptr._data.dat" 1 "original" } }a

! { dg-final { scan-tree-dump-times "sub_array_assumed \\(D" 3 "original" } }
! { dg-final { scan-tree-dump-times " = _gfortran_internal_pack \\(&parm" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(&array_int\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(real\\(kind=4\\).0:. . restrict\\) array_real_alloc.data" 1 "original" } }
! { dg-final { scan-tree-dump-times " = _gfortran_internal_pack \\(&array_char_ptr\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\.data = \\(void .\\) &array_t1.0.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t1.0:. .\\) parm" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t2.0:. . restrict\\) array_t2_alloc.data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t1.0:. . restrict\\) array_class_t1_alloc._data.data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t1.0:. .\\) array_class_t1_ptr._data.data\\);" 1 "original" } }

! { dg-final { cleanup-tree-dump "original" } }
