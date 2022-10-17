! Reported as pr94070.
! { dg-do run }
! { dg-additional-sources "cf-out-descriptor-6-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program checks passing an assumed-size array as an intent(out)
! argument to a bind (c) Fortran function from both C and Fortran.

! Assumed-size arrays are not passed by descriptor.  What we'll do
! for this test function is pass the assumed-size array as the actual
! argument corresponding to an assumed-rank dummy.  This is supposed to
! fill in the descriptor with information about the array present at
! the call site.

subroutine ftest (a, n) bind (c, name="ftest")
  use iso_c_binding
  integer(C_INT), intent(out) :: a(..)
  integer(C_INT), value :: n
  integer :: i

  ! TS 29113
  ! 6.4.2 SIZE
  ! (1) for an assumed-rank object that is associated with an 
  ! assumed-size array, the result has the value −1 if DIM is 
  ! present and equal to the rank of ARRAY
  if (rank (a) .ne. 1) stop 102
  if (size (a, rank (a)) .ne. -1) stop 100
  if (lbound (a, rank (a)) .ne. 1) stop 101

  select rank (a)
    rank (*)
      do i = 1, n
        a(i) = i
      end do
    rank default
      stop 102
  end select
end subroutine

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (a, n) bind (c, name="ctest")
      use iso_c_binding
      integer(C_INT), intent(out) :: a(..)
      integer(C_INT), value :: n
    end subroutine
    subroutine ftest (a, n) bind (c, name="ftest")
      use iso_c_binding
      integer(C_INT), intent(out) :: a(..)
      integer(C_INT), value :: n
    end subroutine
  end interface

  integer(C_INT), target :: aa(10)

  ! To get an assumed-size array descriptor, we have to first pass the
  ! fixed-size array to a Fortran function with an assumed-size dummy,
  call ftest1 (aa, 10)  ! calls ftest
  call ftest2 (aa, 10)  ! has c binding, calls ftest
  call ftest3 (aa, 10)  ! calls ctest -> ftest
  call ftest4 (aa, 10)  ! has c binding, calls ctest -> ftest

contains

  subroutine ftest1 (a, n)
    use iso_c_binding
    integer(C_INT), intent(out) :: a(*)
    integer(C_INT), value :: n
    integer :: i
    a(1:n) = 0
    call ftest (a, n)
    do i = 1, n
      if (a (i) .ne. i) stop 200
    end do
  end subroutine

  subroutine ftest2 (a, n) bind (c)
    use iso_c_binding
    integer(C_INT), intent(out) :: a(*)
    integer(C_INT), value :: n
    integer :: i
    a(1:n) = 0
    call ftest (a, n)
    do i = 1, n
      if (a (i) .ne. i) stop 201
    end do
  end subroutine

  subroutine ftest3 (a, n)
    use iso_c_binding
    integer(C_INT), intent(out) :: a(*)
    integer(C_INT), value :: n
    integer :: i
    a(1:n) = 0
    call ctest (a, n)
    do i = 1, n
      if (a (i) .ne. i) stop 202
    end do
  end subroutine

  subroutine ftest4 (a, n) bind (c)
    use iso_c_binding
    integer(C_INT), intent(out) :: a(*)
    integer(C_INT), value :: n
    integer :: i
    a(1:n) = 0
    call ctest (a, n)
    do i = 1, n
      if (a (i) .ne. i) stop 203
    end do
  end subroutine
end program
