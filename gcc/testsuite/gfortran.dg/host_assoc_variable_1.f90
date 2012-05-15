! { dg-do compile }
! This tests that PR32760, in its various manifestations is fixed.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
! This is the original bug - the frontend tried to fix the flavor of
! 'PRINT' too early so that the compile failed on the subroutine 
! declaration.
!
module gfcbug68
  implicit none
  public :: print
contains
  subroutine foo (i)
    integer, intent(in)  :: i
    print *, i
  end subroutine foo
  subroutine print (m)
    integer, intent(in) :: m
  end subroutine print
end module gfcbug68

! This version of the bug appears in comment # 21.
!
module m
  public :: volatile
contains
  subroutine foo
    volatile :: bar
  end subroutine foo
  subroutine volatile
  end subroutine volatile
end module

! This was a problem with the resolution of the STAT parameter in 
! ALLOCATE and DEALLOCATE that was exposed in comment #25.
!
module n
  public :: integer
  private :: istat
contains
  subroutine foo
    integer, allocatable :: s(:), t(:)
    allocate(t(5))
    allocate(s(4), stat=istat)
  end subroutine foo
  subroutine integer()
  end subroutine integer
end module n

! This is the version of the bug in comment #12 of the PR.
!
module gfcbug68a
  implicit none
  public :: write
contains
  function foo (i)
    integer, intent(in)  :: i
    integer foo
    write (*,*) i
    foo = i
  end function foo
  subroutine write (m)
    integer, intent(in) :: m
    print *, m*m*m
  end subroutine write
end module gfcbug68a

program testit
  use gfcbug68a
  integer :: i = 27
  integer :: k
  k = foo(i)
  print *, "in the main:", k
  call write(33)
end program testit
