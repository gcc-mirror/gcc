! { dg-do run }
! { dg-additional-sources "optional-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! TS 29113
! 8.7 An absent actual argument in a reference to an interoperable
! procedure is indicated by a corresponding formal parameter with the
! value of a null pointer. An absent optional dummy argument in a
! reference to an interoperable procedure from a C function is indicated
! by a corresponding argument with the value of a null pointer.

module m
  use iso_c_binding
  integer(C_INT) :: aa(32)
  integer(C_INT) :: bb
  character(C_CHAR) :: cc
  real(C_DOUBLE) :: dd
end module

subroutine ftest (n, a, b, c, d) bind (c)
  use iso_c_binding
  use m
  implicit none
  integer(C_INT), value :: n
  integer(C_INT), optional :: a(:)
  integer(C_INT), optional :: b
  character(C_CHAR), optional :: c
  real(C_DOUBLE), optional :: d

  if (n .ge. 1) then
    if (.not. present (a)) stop 101
    if (any (a .ne. aa)) stop 201
  else
    if (present (a)) stop 301
  end if

  if (n .ge. 2) then
    if (.not. present (b)) stop 102
    if (b .ne. bb) stop 201
  else
    if (present (b)) stop 302
  end if

  if (n .ge. 3) then
    if (.not. present (c)) stop 103
    if (c .ne. cc) stop 201
  else
    if (present (c)) stop 303
  end if

  if (n .ge. 4) then
    if (.not. present (d)) stop 104
    if (d .ne. dd) stop 201
  else
    if (present (d)) stop 304
  end if
end subroutine

program testit
  use iso_c_binding
  use m
  implicit none

  interface
    subroutine ctest1 (a, b, c, d) bind (c)
      use iso_c_binding
      integer(C_INT) :: a(:)
      integer(C_INT) :: b
      character(C_CHAR) :: c
      real(C_DOUBLE) :: d
    end subroutine
    subroutine ctest2 (n, a, b, c, d) bind (c)
      use iso_c_binding
      integer(C_INT), value :: n
      integer(C_INT), optional :: a(:)
      integer(C_INT), optional :: b
      character(C_CHAR), optional :: c
      real(C_DOUBLE), optional :: d
    end subroutine
    subroutine ftest (n, a, b, c, d) bind (c)
      use iso_c_binding
      integer(C_INT), value :: n
      integer(C_INT), optional :: a(:)
      integer(C_INT), optional :: b
      character(C_CHAR), optional :: c
      real(C_DOUBLE), optional :: d
    end subroutine
  end interface


  ! Initialize the variables above.
  integer :: i
  do i = 1, 32
    aa(i) = i
  end do
  bb = 42
  cc = '$'
  dd = acos(-1.D0)

  call ftest (0)
  call ftest (1, aa)
  call ftest (2, aa, bb)
  call ftest (3, aa, bb, cc)
  call ftest (4, aa, bb, cc, dd)

  call ctest1 (aa, bb, cc, dd)
  call ctest2 (0)
  call ctest2 (1, aa)
  call ctest2 (2, aa, bb)
  call ctest2 (3, aa, bb, cc)
  call ctest2 (4, aa, bb, cc, dd)

end program

