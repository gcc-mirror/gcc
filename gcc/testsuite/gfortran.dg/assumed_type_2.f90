! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/48820
!
! Test TYPE(*)
!

module mod
  use iso_c_binding, only: c_loc, c_ptr, c_bool
  implicit none
  interface my_c_loc
    function my_c_loc1(x) bind(C)
      import c_ptr
      type(*) :: x
      type(c_ptr) :: my_c_loc1
    end function
    function my_c_loc2(x) bind(C)
      import c_ptr
      type(*) :: x(*)
      type(c_ptr) :: my_c_loc2
    end function
  end interface my_c_loc
contains
  subroutine sub_scalar (arg1, presnt)
     type(*), target, optional :: arg1
     logical :: presnt
     type(c_ptr) :: cpt
     if (presnt .neqv. present (arg1)) call abort ()
     cpt = c_loc (arg1)
  end subroutine sub_scalar

  subroutine sub_array_shape (arg2, lbounds, ubounds)
     type(*), target :: arg2(:,:)
     type(c_ptr) :: cpt
     integer :: lbounds(2), ubounds(2)
     if (any (lbound(arg2) /= lbounds)) call abort ()
     if (any (ubound(arg2) /= ubounds)) call abort ()
     if (any (shape(arg2) /= ubounds-lbounds+1)) call abort ()
     if (size(arg2) /= product (ubounds-lbounds+1)) call abort ()
     if (rank (arg2) /= 2) call abort ()
!     if (.not. is_continuous (arg2)) call abort () !<< Not yet implemented
!     cpt = c_loc (arg2) ! << FIXME: Valid since TS29113
     call sub_array_assumed (arg2)
  end subroutine sub_array_shape

  subroutine sub_array_assumed (arg3)
     type(*), target :: arg3(*)
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

call sub_array_shape (array_real_alloc, [1,1], shape(array_real_alloc))
call sub_array_shape (array_char_ptr, [1,1], shape(array_char_ptr))
call sub_array_shape (array_t2_alloc, [1,1], shape(array_t2_alloc))
call sub_array_shape (array_t3_ptr, [1,1], shape(array_t3_ptr))
call sub_array_shape (array_class_t1_alloc, [1,1], shape(array_class_t1_alloc))
call sub_array_shape (array_class_t1_ptr, [1,1], shape(array_class_t1_ptr))

deallocate (scalar_char_ptr, scalar_class_t1_ptr, array_char_ptr)
deallocate (array_class_t1_ptr, array_t3_ptr)

end

! { dg-final { cleanup-modules "mod" } }

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

! { dg-final { scan-tree-dump-times "sub_array_assumed \\(D" 2 "original" } }
! { dg-final { scan-tree-dump-times " = _gfortran_internal_pack \\(&parm" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(&array_int\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(real\\(kind=4\\).0:. . restrict\\) array_real_alloc.data" 1 "original" } }
! { dg-final { scan-tree-dump-times " = _gfortran_internal_pack \\(&array_char_ptr\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "\\.data = \\(void .\\) &array_t1.0.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t1.0:. .\\) parm" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t2.0:. . restrict\\) array_t2_alloc.data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t3.0:. .\\) array_t3_ptr.data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t1.0:. . restrict\\) array_class_t1_alloc._data.data\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_assumed \\(\\(struct t1.0:. .\\) array_class_t1_ptr._data.data\\);" 1 "original" } }

! { dg-final { scan-tree-dump-times "sub_array_shape \\(&array_real_alloc," 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_shape \\(&array_char_ptr," 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_shape \\(&array_t2_alloc," 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_shape \\(&array_t3_ptr," 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_shape \\(&array_class_t1_alloc._data," 1 "original" } }
! { dg-final { scan-tree-dump-times "sub_array_shape \\(&array_class_t1_ptr._data," 1 "original" } }

! { dg-final { cleanup-tree-dump "original" } }
