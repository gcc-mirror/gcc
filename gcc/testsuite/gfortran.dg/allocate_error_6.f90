! { dg-do run }
! { dg-additional-options "-fcheck=mem" }
! { dg-shouldfail "Fortran runtime error: Assignment of scalar to unallocated array" }
!
! This omission was encountered in the course of fixing PR54070. Whilst this is a
! very specific case, others such as allocatable components have been tested.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
function g(a) result (res)
  real :: a
  real,allocatable :: res(:)
  res = a  ! Since 'res' is not allocated, a runtime error should occur.
end function

  interface
    function g(a) result(res)
      real :: a
      real,allocatable :: res(:)
    end function
  end interface
!  print *, g(2.0)
!  call foo
  call foofoo
contains
  subroutine foo
    type bar
      real, allocatable, dimension(:) :: r
    end type
    type (bar) :: foobar
    foobar%r = 1.0
  end subroutine
  subroutine foofoo
    type barfoo
      character(:), allocatable, dimension(:) :: c
    end type
    type (barfoo) :: foobarfoo
    foobarfoo%c = "1.0"
  end subroutine
end
