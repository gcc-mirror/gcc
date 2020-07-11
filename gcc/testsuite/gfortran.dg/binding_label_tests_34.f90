! { dg-do compile }
! PR 94737 - global symbols are case-insensitive; an error should be
! reported if they match (see F2018, 9.2, paragraph 2).  Original
! test case by Lee Busby.

module foo

interface
function func1(ii) result (k) bind(c, name="c_func")
  integer :: ii
  integer :: k
end function func1
subroutine sub1(ii,jj) bind(c, name="c_Func") ! { dg-error "Global binding name" }
  integer :: ii,jj
end subroutine sub1
end interface

contains

function func2(ii) result (k) 
  integer :: ii
  integer :: k
  k = func1(ii) ! { dg-error "Global binding name" }
end function func2
end module foo
