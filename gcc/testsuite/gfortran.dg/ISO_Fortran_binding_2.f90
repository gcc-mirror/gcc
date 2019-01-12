! { dg-do run }
! { dg-additional-sources ISO_Fortran_binding_2.c }
! { dg-options "-fbounds-check" }
!
! Test F2018 18.5: ISO_Fortran_binding.h function errors.
!
  USE, INTRINSIC :: ISO_C_BINDING

  TYPE, BIND(C) :: T
    REAL(C_DOUBLE) :: X
    complex(C_DOUBLE_COMPLEX) :: Y
  END TYPE

  type :: mytype
    integer :: i
    integer :: j
  end type

  INTERFACE
    FUNCTION c_address(a, idx) BIND(C, NAME="address_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      INTEGER(C_INT), dimension(1) :: idx
      type(*), DIMENSION(..) :: a
    END FUNCTION c_address

    FUNCTION c_deallocate(a) BIND(C, NAME="deallocate_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      type(*), DIMENSION(..) :: a
    END FUNCTION c_deallocate

    FUNCTION c_allocate(a, lower, upper) BIND(C, NAME="allocate_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      type(*), DIMENSION(..) :: a
      integer(C_INTPTR_T), DIMENSION(15) :: lower, upper
    END FUNCTION c_allocate

    FUNCTION c_establish(a, rank, attr) BIND(C, NAME="establish_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      import
      INTEGER(C_INT) :: err
      INTEGER(C_INT) :: rank, attr
      type (T), DIMENSION(..), intent(out) :: a
    END FUNCTION c_establish

    FUNCTION c_contiguous(a) BIND(C, NAME="contiguous_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      type(*), DIMENSION(..) :: a
    END FUNCTION c_contiguous

    FUNCTION c_section(std_case, a, lower, strides) BIND(C, NAME="section_c") RESULT(ans)
      USE, INTRINSIC :: ISO_C_BINDING
      real(C_FLOAT) :: ans
      INTEGER(C_INT) :: std_case
      INTEGER(C_INT), dimension(15) :: lower
      INTEGER(C_INT), dimension(15) :: strides
      type(*), DIMENSION(..) :: a
    END FUNCTION c_section

    FUNCTION c_select_part(a) BIND(C, NAME="select_part_c") RESULT(ans)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: ans
      type(*), DIMENSION(..) :: a
    END FUNCTION c_select_part

    FUNCTION c_setpointer(a, b, lbounds) BIND(C, NAME="setpointer_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      INTEGER(C_INT), dimension(2) :: lbounds
      type(*), DIMENSION(..) :: a, b
    END FUNCTION c_setpointer
  END INTERFACE

  integer(C_INTPTR_T), dimension(15) :: lower, upper

  call test_CFI_address
  call test_CFI_deallocate
  call test_CFI_allocate
  call test_CFI_establish
  call test_CFI_contiguous
  call test_CFI_section
  call test_CFI_select_part
  call test_CFI_setpointer

contains
  subroutine test_CFI_address
    integer, dimension(:), allocatable :: a
    allocate (a, source = [1,2,3])
    if (c_address (a, [2]) .ne. 3) stop 1   ! OK
    if (c_address (a, [3]) .ne. -1) stop 2  ! "subscripts[0], is out of bounds"
    if (c_address (a, [-1]) .ne. -1) stop 3 ! "subscripts[0], is out of bounds"
    deallocate (a)
    if (c_address (a, [2]) .ne. -1) stop 4  ! "C Descriptor must not be NULL"
  end subroutine test_CFI_address

  subroutine test_CFI_deallocate
    integer, dimension(:), allocatable :: a
    integer, dimension(2,2) :: b
    if (c_deallocate (a) .ne. 2) stop 5     ! "Base address is already NULL"
    allocate (a(2))
    if (c_deallocate (a) .ne. 0) stop 6     ! OK
    if (c_deallocate (b) .ne. 7) stop 7     ! "must describe a pointer or allocatable"
  end subroutine test_CFI_deallocate

  subroutine test_CFI_allocate
    integer, dimension(:,:), allocatable :: a
    integer, dimension(2,2) :: b
    lower(1:2) = [2,2]
    upper(1:2) = [10,10]
    allocate (a(1,1))
    if (c_allocate (a, lower, upper) .ne. 3) stop 8  ! "C descriptor must be NULL"
    if (allocated (a)) deallocate (a)
    if (c_allocate (a, lower, upper) .ne. 0) stop 9  ! OK
    if (c_allocate (b, lower, upper) .ne. 7) STOP 10 ! "must describe a pointer or allocatable"
  end subroutine test_CFI_allocate

  subroutine test_CFI_establish
    type(T), allocatable :: a(:)
    INTEGER(C_INT) :: rank
    INTEGER(C_INT) :: attr
    attr = 0                                         ! establish a pointer
    rank = 16
    if (c_establish (a, rank, attr) .ne. 5) stop 11  ! "Rank must be between 0 and 15"
    rank = 1
    if (c_establish (a, rank, attr) .ne. 0) stop 12  ! OK
    if (allocated (a)) deallocate (a)
    if (c_establish (a, rank, attr) .ne. 0) Stop 13  ! OK the first time
    if (c_establish (a, rank, attr) .ne. 10) Stop 14 ! "its base address must be NULL"
    if (allocated (a)) deallocate (a)
    attr = 1                                         ! establish an allocatable
    if (c_establish (a, rank, attr) .ne. 7) Stop 15  ! "is for a nonallocatable entity"
  end subroutine test_CFI_establish

  subroutine test_CFI_contiguous
    integer, allocatable :: a
    if (c_contiguous (a) .ne. 2) stop 16  ! "Descriptor is already NULL"
    allocate (a)
    if (c_contiguous (a) .ne. 5) stop 17  ! "must describe an array"
  end subroutine test_CFI_contiguous

  subroutine test_CFI_section
    real, allocatable, dimension (:) :: a
    integer, dimension(15) :: lower, strides
    integer :: i
    real :: b
    lower(1) = 10
    strides(1) = 5
    if (int (c_section (1, a, lower, strides)) .ne. 2) &
        stop 18 ! "Base address of source must not be NULL"
    allocate (a(100))
    if (int (c_section (1, a, lower, strides)) .ne. 0) &
        stop 19 ! OK
    if (int (c_section (1, b, lower, strides)) .ne. 5) &
        stop 20 ! "Source must describe an array"
    strides(1) = 0
    if (int (c_section (1, a, lower, strides)) .ne. 5) &
        stop 21 ! "Rank of result must be equal to the rank of source"
    strides(1) = 5
    lower(1) = -1
    if (int (c_section (1, a, lower, strides)) .ne. 12) &
        stop 22 ! "Lower bounds must be within the bounds of the fortran array"
    lower(1) = 100
    if (int (c_section (1, a, lower, strides)) .ne. 12) &
        stop 23 ! "Lower bounds must be within the bounds of the fortran array"
  end subroutine test_CFI_section

  subroutine test_CFI_select_part
    type(t), allocatable, dimension(:) :: a
    type(t) :: src
    allocate (a(1), source = src)
    if (c_select_part (a) .ne. 5) stop 24 ! "Source and result must have the same rank"
    deallocate (a)
    if (c_select_part (a) .ne. 2) stop 25 ! "source must not be NULL"
  end subroutine test_CFI_select_part

  subroutine test_CFI_setpointer
    integer, dimension(2,2), target :: tgt1
    integer, dimension(:,:), pointer :: src
    type (t), dimension(2), target :: tgt2
    type (t), dimension(:), pointer :: res
    type (t), dimension(2, 2), target, save :: tgt3
    type (t), dimension(:, :), pointer :: src1
    integer, dimension(2) :: lbounds = [-1, -2]
    src => tgt1
    res => tgt2
    if (c_setpointer (res, src, lbounds) .ne. 4) stop 26 ! "Element lengths"
    src1 => tgt3
    if (c_setpointer (res, src1, lbounds) .ne. 5) stop 27 ! "Ranks of result"
  end subroutine test_CFI_setpointer
end
