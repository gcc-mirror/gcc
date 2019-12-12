! { dg-do run }
! Tests the patch that implements F2003 automatic allocation and
! reallocation of allocatable arrays on assignment.  The tests
! below were generated in the final stages of the development of
! this patch.
! test1 has been corrected for PR47051
!
! Contributed by Dominique Dhumieres <dominiq@lps.ens.fr>
!            and Tobias Burnus <burnus@gcc.gnu.org>
!
  integer :: nglobal
  call test1
  call test2
  call test3
  call test4
  call test5
  call test6
  call test7
  call test8
contains
  subroutine test1
!
! Check that the bounds are set correctly, when assigning
! to an array that already has the correct shape.
!
    real :: a(10) = 1, b(51:60) = 2
    real, allocatable :: c(:), d(:)
    c=a
    if (lbound (c, 1) .ne. lbound(a, 1)) STOP 1
    if (ubound (c, 1) .ne. ubound(a, 1)) STOP 2
    c=b
! 7.4.1.3 "If variable is an allocated allocatable variable, it is
! deallocated if expr is an array of different shape or any of the
! corresponding length type parameter values of variable and expr
! differ." Here the shape is the same so the deallocation does not
! occur and the bounds are not recalculated. This was corrected
! for the fix of PR47051. 
    if (lbound (c, 1) .ne. lbound(a, 1)) STOP 3
    if (ubound (c, 1) .ne. ubound(a, 1)) STOP 4
    d=b
    if (lbound (d, 1) .ne. lbound(b, 1)) STOP 5
    if (ubound (d, 1) .ne. ubound(b, 1)) STOP 6
    d=a
! The other PR47051 correction.
    if (lbound (d, 1) .ne. lbound(b, 1)) STOP 7
    if (ubound (d, 1) .ne. ubound(b, 1)) STOP 8
  end subroutine
  subroutine test2
!
! Check that the bounds are set correctly, when making an
! assignment with an implicit conversion.  First with a
! non-descriptor variable....
!
    integer(4), allocatable :: a(:)
    integer(8) :: b(5:6)
    a = b
    if (lbound (a, 1) .ne. lbound(b, 1)) STOP 9
    if (ubound (a, 1) .ne. ubound(b, 1)) STOP 10
  end subroutine
  subroutine test3
!
! ...and now a descriptor variable.
!
    integer(4), allocatable :: a(:)
    integer(8), allocatable :: b(:)
    allocate (b(7:11))
    a = b
    if (lbound (a, 1) .ne. lbound(b, 1)) STOP 11
    if (ubound (a, 1) .ne. ubound(b, 1)) STOP 12
  end subroutine
  subroutine test4
!
! Check assignments of the kind a = f(...)
!
    integer, allocatable :: a(:)
    integer, allocatable :: c(:)
    a = f()
    if (any (a .ne. [1, 2, 3, 4])) STOP 13
    c = a + 8
    a = f (c)
    if (any ((a - 8) .ne. [1, 2, 3, 4])) STOP 14
    deallocate (c)
    a = f (c)
    if (any ((a - 4) .ne. [1, 2, 3, 4])) STOP 15
  end subroutine
  function f(b)
    integer, allocatable, optional :: b(:)
    integer :: f(4)
    if (.not.present (b)) then
      f = [1,2,3,4]
    elseif (.not.allocated (b)) then
      f = [5,6,7,8]
    else
      f = b
    end if
  end function f
  
  subroutine test5
!
! Extracted from rnflow.f90, Polyhedron benchmark suite,
! http://www.polyhedron.com
!
    integer, parameter :: ncls = 233, ival = 16, ipic = 17
    real, allocatable, dimension (:,:) :: utrsft
    real, allocatable, dimension (:,:) :: dtrsft
    real, allocatable, dimension (:,:) :: xwrkt
    allocate (utrsft(ncls, ncls), dtrsft(ncls, ncls))
    nglobal = 0
    xwrkt = trs2a2 (ival, ipic, ncls)
    if (any (shape (xwrkt) .ne. [ncls, ncls])) STOP 16
    xwrkt = invima (xwrkt, ival, ipic, ncls)
    if (nglobal .ne. 1) STOP 17
    if (sum(xwrkt) .ne. xwrkt(ival, ival)) STOP 18
  end subroutine
  function trs2a2 (j, k, m)
    real, dimension (1:m,1:m) :: trs2a2
    integer, intent (in)      :: j, k, m
    nglobal = nglobal + 1
    trs2a2 = 0.0
  end function trs2a2
  function invima (a, j, k, m)
    real, dimension (1:m,1:m)              :: invima
    real, dimension (1:m,1:m), intent (in) :: a
    integer, intent (in)            :: j, k
    invima = 0.0
    invima (j, j) = 1.0 / (1.0 - a (j, j))
  end function invima
  subroutine test6
    character(kind=1, len=100), allocatable, dimension(:) :: str
    str = [ "abc" ]
    if (TRIM(str(1)) .ne. "abc") STOP 19
    if (len(str) .ne. 100) STOP 20
  end subroutine
  subroutine test7
    character(kind=4, len=100), allocatable, dimension(:) :: str
    character(kind=4, len=3) :: test = "abc"
    str = [ "abc" ]
    if (TRIM(str(1)) .ne. test) STOP 21
    if (len(str) .ne. 100) STOP 22
  end subroutine
  subroutine test8
    type t
      integer, allocatable :: a(:)
    end type t
    type(t) :: x
    x%a= [1,2,3]
    if (any (x%a .ne. [1,2,3])) STOP 23
    x%a = [4]
    if (any (x%a .ne. [4])) STOP 24
  end subroutine
end

