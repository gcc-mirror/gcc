! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_1.c }
!
! Test F2008 18.5: ISO_Fortran_binding.h functions.
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
    FUNCTION elemental_mult(a, b, c) BIND(C, NAME="elemental_mult_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      type(*), DIMENSION(..) :: a, b, c
    END FUNCTION elemental_mult

    FUNCTION c_deallocate(a) BIND(C, NAME="deallocate_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      INTEGER(C_INT), DIMENSION(..), allocatable :: a
    END FUNCTION c_deallocate

    FUNCTION c_allocate(a, lower, upper) BIND(C, NAME="allocate_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      INTEGER(C_INT), DIMENSION(..), allocatable :: a
      integer(C_INTPTR_T), DIMENSION(15) :: lower, upper
    END FUNCTION c_allocate

    FUNCTION c_establish(a) BIND(C, NAME="establish_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      import
      INTEGER(C_INT) :: err
      type (T), pointer, DIMENSION(..), intent(out) :: a
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
      real(C_DOUBLE) :: ans
      type(*), DIMENSION(..) :: a
    END FUNCTION c_select_part

    FUNCTION c_setpointer(a, lbounds) BIND(C, NAME="setpointer_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      INTEGER(C_INT), dimension(2) :: lbounds
      INTEGER(C_INT), DIMENSION(..), pointer :: a
    END FUNCTION c_setpointer

    FUNCTION c_assumed_size(a) BIND(C, NAME="assumed_size_c") RESULT(err)
      USE, INTRINSIC :: ISO_C_BINDING
      INTEGER(C_INT) :: err
      type(*), DIMENSION(..) :: a
    END FUNCTION c_assumed_size

  END INTERFACE

  integer, dimension(:,:), allocatable :: x, y, z
  integer, dimension(2,2) :: a, b, c
  integer, dimension(4,4) :: d
  integer :: i = 42, j, k
  integer(C_INTPTR_T), dimension(15) :: lower, upper
  real, dimension(10,10) :: arg
  type (mytype), dimension(2,2) :: der

  allocate (x, source = reshape ([4,3,2,1], [2,2]))
  allocate (y, source = reshape ([2,3,4,5], [2,2]))
  allocate (z, source = reshape ([0,0,0,0], [2,2]))

  call test_CFI_address
  call test_CFI_deallocate
  call test_CFI_allocate
  call test_CFI_establish
  call test_CFI_contiguous (a)
  call test_CFI_section (arg)
  call test_CFI_select_part
  call test_CFI_setpointer
  call test_assumed_size (a)
contains
  subroutine test_CFI_address
! Basic test that CFI_desc_t can be passed and that CFI_address works
    if (elemental_mult (z, x, y) .ne. 0) stop 1
    if (any (z .ne. reshape ([8,9,8,5], [2,2]))) stop 2

    a = reshape ([4,3,2,1], [2,2])
    b = reshape ([2,3,4,5], [2,2])
    c = 0
! Verify that components of arrays of derived types are OK.
    der%j = a
! Check that non-pointer/non-allocatable arguments are OK
    if (elemental_mult (c, der%j, b) .ne. 0) stop 3
    if (any (c .ne. reshape ([8,9,8,5], [2,2]))) stop 4

! Check array sections
    d = 0
    d(4:2:-2, 1:3:2) = b
    if (elemental_mult (c, a, d(4:2:-2, 1:3:2)) .ne. 0) stop 5
    if (any (c .ne. reshape ([8,9,8,5], [2,2]))) stop 6

! If a scalar result is passed to 'elemental_mult' it is returned
! as the function result and then zeroed. This tests that scalars
! are correctly converted to CF_desc_t.
    if ((elemental_mult (i, a, b) .ne. 42) &
        .or. (i .ne. 0)) stop 7
    deallocate (y,z)
end subroutine test_CFI_address

  subroutine test_CFI_deallocate
! Test CFI_deallocate.
    if (c_deallocate (x) .ne. 0) stop 8
    if (allocated (x)) stop 9
  end subroutine test_CFI_deallocate

  subroutine test_CFI_allocate
! Test CFI_allocate.
    lower(1:2) = [2,2]
    upper(1:2) = [10,10]

    if (c_allocate (x, lower, upper) .ne. 0) stop 10
    if (.not.allocated (x)) stop 11
    if (any (lbound (x) .ne. lower(1:2))) stop 12
    if (any (ubound (x) .ne. upper(1:2))) stop 13

! Elements are filled by 'c_allocate' with the product of the fortran indices
    do j = lower(1) , upper(1)
      do k = lower(2) , upper(2)
        x(j,k) = x(j,k) - j * k
      end do
    end do
    if (any (x .ne. 0)) stop 14
    deallocate (x)
  end subroutine test_CFI_allocate

  subroutine test_CFI_establish
! Test CFI_establish.
    type(T), pointer :: case2(:) => null()
    if (c_establish(case2) .ne. 0) stop 14
    if (ubound(case2, 1) .ne. 9) stop 15
    if (.not.associated(case2)) stop 16
    if (sizeof(case2) .ne. 240) stop 17
    if (int (sum (case2%x)) .ne. 55) stop 18
    if (int (sum (imag (case2%y))) .ne. 110) stop 19
    deallocate (case2)
  end subroutine test_CFI_establish

  subroutine test_CFI_contiguous (arg)
    integer, dimension (2,*) :: arg
    character(4), dimension(2) :: chr
! These are contiguous
    if (c_contiguous (arg) .ne. 1) stop 20
    if (.not.allocated (x)) allocate (x(2, 2))
    if (c_contiguous (x) .ne. 1) stop 22
    deallocate (x)
    if (c_contiguous (chr) .ne. 1) stop 23
! These are not contiguous
    if (c_contiguous (der%i) .eq. 1) stop 24
    if (c_contiguous (arg(1:1,1:2)) .eq. 1) stop 25
    if (c_contiguous (d(4:2:-2, 1:3:2)) .eq. 1) stop 26
    if (c_contiguous (chr(:)(2:3)) .eq. 1) stop 27
  end subroutine test_CFI_contiguous

  subroutine test_CFI_section (arg)
    real, dimension (100) :: a
    real, dimension (10,*) :: arg
    integer, dimension(15) :: lower, strides
    integer :: i

! Case (i) from F2018:18.5.5.7.
    a = [(real(i), i = 1, 100)]
    lower(1) = 10
    strides(1) = 5
! Remember, 'a' being non pointer, non-allocatable, the C descriptor
! lbounds are set to zero.
    if (int (sum(a(lower(1)+1::strides(1))) &
             - c_section(1, a, lower, strides)) .ne. 0) stop 28
! Case (ii) from F2018:18.5.5.7.
    arg(:,1:10) = reshape ([(real(i), i = 1, 100)], [10,10])
    lower(1) = 1
    lower(2) = 5
    strides(1) = 1
    strides(2) = 0
    if (int (sum(arg(:,5)) &
             - c_section (2, arg, lower, strides)) .ne. 0) stop 29
  end subroutine test_CFI_section

  subroutine test_CFI_select_part
! Test the example from F2018:18.5.5.8.
! Modify to take rank 2 and sum the section type_t(5, :)%y%im
! Note that sum_z_5 = sum (type_t(5, :)%y%im) is broken on Darwin.
!
    type (t), dimension(10, 10) :: type_t
    real(kind(type_t%x)) :: v, sum_z_5 = 0.0
    complex(kind(type_t%y)) :: z
! Set the array 'type_t'.
    do j = 1, 10
      do k = 1, 10
        v = dble (j * k)
        z = cmplx (2 * v, 3 * v)
        type_t(j, k) = t (v, z)
        if (j .eq. 5) sum_z_5 = sum_z_5 + imag (z)
      end do
    end do
! Now do the test.
    if (int (c_select_part (type_t) - sum_z_5) .ne. 0) stop 30
  end subroutine test_CFI_select_part

  subroutine test_CFI_setpointer
! Test the example from F2018:18.5.5.9.
    integer, dimension(:,:), pointer :: ptr => NULL ()
    integer, dimension(2,2), target :: tgt
    integer, dimension(2) :: lbounds = [-1, -2]
! The C-function resets the lbounds
    ptr(1:, 1:) => tgt
    if (c_setpointer (ptr, lbounds) .ne. 0) stop 31
    if (any (lbound(ptr) .ne. lbounds)) stop 32
  end subroutine test_CFI_setpointer

  subroutine test_assumed_size (arg)
    integer, dimension(2,*) :: arg
! The C-function checks contiguousness and that extent[1] == -1.
    if (c_assumed_size (arg) .ne. 0) stop 33
  end subroutine
end
