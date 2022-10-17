! PR 93308
! { dg-do run }
! { dg-additional-sources "cf-descriptor-2-c.c dump-descriptors.c" }
!
! This program checks that building a descriptor for a fixed-size array
! in C works and that you can use it to call back into a Fortran function
! declared to have c binding, as an assumed-rank argument.
! 
! Fixed by
! https://gcc.gnu.org/pipermail/gcc-patches/2021-June/572725.html

module mm
  use iso_c_binding
  type, bind (c) :: m
    integer(C_INT) :: i, j
  end type

  integer, parameter :: imax=10, jmax=5
end module

subroutine ftest (a, b) bind (c, name="ftest")
  use iso_c_binding
  use mm
  type(m) :: a(..), b(..)
  integer :: i, j

  select rank (a)
    rank (2)
      select rank (b)
        rank (2)
	  ! print *, lbound(a,1), ubound(a,1), lbound(a,2), ubound(a,2)
	  ! print *, lbound(b,1), ubound(b,1), lbound(b,2), ubound(b,2)
          if (lbound (a,1) .ne. 1 .or. ubound (a,1) .ne. imax) stop 101
          if (lbound (a,2) .ne. 1 .or. ubound (a,2) .ne. jmax) stop 102
          if (lbound (b,1) .ne. 1 .or. ubound (b,1) .ne. jmax) stop 103
          if (lbound (b,2) .ne. 1 .or. ubound (b,2) .ne. imax) stop 104
	  do j = 1, jmax
	    do i = 1, imax
	      print *, a(i,j)%i, a(i,j)%j, b(j,i)%i, b(j,i)%j
	      if (a(i,j)%i .ne. i) stop 105
	      if (a(i,j)%j .ne. j) stop 106
	      if (b(j,i)%i .ne. i) stop 107
	      if (b(j,i)%j .ne. j) stop 108
            end do
	  end do
        rank default
          stop 106
      end select
    rank default
      stop 107
  end select
end subroutine


program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ctest (a) bind (c)
      use iso_c_binding
      use mm
      type(m) :: a(..)
    end subroutine
  end interface

  type(m) :: aa(imax,jmax)
  integer :: i, j
  do j = 1, jmax
    do i = 1, imax
      aa(i,j)%i = i
      aa(i,j)%j = j
    end do
  end do

  ! Pass the initialized array to a C function ctest, which will generate its
  ! transpose and call ftest with it.

  call ctest (aa)

end program
