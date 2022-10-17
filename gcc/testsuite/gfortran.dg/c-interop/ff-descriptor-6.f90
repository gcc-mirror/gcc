! Reported as pr94070.
! { dg-do run }
!
! This program checks that passing assumed-size arrays to
! and from Fortran functions with C binding works.
!

program testit
  use iso_c_binding
  implicit none

  ! Assumed-size arrays are not passed by descriptor.  What we'll do
  ! for this test function is bind an assumed-rank dummy
  ! to the assumed-size array.  This is supposed to fill in the descriptor
  ! with information about the array present at the call site.
  interface
    subroutine ctest (a) bind (c)
      use iso_c_binding
      integer(C_INT) :: a(..)
    end subroutine
  end interface

  integer(C_INT), target :: aa(10,5:8)

  ! To get an assumed-size array descriptor, we have to first pass the
  ! fixed-size array to a Fortran function with an assumed-size dummy,
  call ftest1 (aa)
  call ftest2 (aa)
  call ftest3 (aa)

contains
  subroutine ftest1 (a)
    use iso_c_binding
    integer(C_INT) :: a(10,*)
    call testf (a)
    call testc (a)
  end subroutine
  subroutine ftest2 (a)
    use iso_c_binding
    integer(C_INT) :: a(10,5:*)
    call testf (a)
    call testc (a)
  end subroutine
  subroutine ftest3 (a) bind (c)
    use iso_c_binding
    integer(C_INT) :: a(10,1:*)
    call testf (a)
    call testc (a)
  end subroutine

  subroutine testf (a)
    use iso_c_binding
    integer(C_INT) :: a(..)
    if (rank (a) .ne. 2)  stop 101
    print *, size (a, 1), size (a, 2)
    if (size (a, 1) .ne. 10) stop 102
    if (size (a, 2) .ne. -1) stop 103
    if (any (lbound (a) .eq. 0)) stop 104
  end subroutine

  subroutine testc (a) bind (c)
    use iso_c_binding
    integer(C_INT) :: a(..)
    if (rank (a) .ne. 2)  stop 201
    print *, size (a, 1), size (a, 2)
    if (size (a, 1) .ne. 10) stop 202
    if (size (a, 2) .ne. -1) stop 203
    if (any (lbound (a) .eq. 0)) stop 204
  end subroutine

end program
