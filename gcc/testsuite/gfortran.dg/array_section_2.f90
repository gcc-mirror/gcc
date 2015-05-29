! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR38033 - size(a) was not stabilized correctly and so the expression was
! evaluated twice outside the loop and then within the scalarization loops.
!
! Contributed by Thomas Bruel  <tmbdev@gmail.com>
!
program test
   integer, parameter :: n = 100
   real, pointer :: a(:),temp(:)  ! pointer or allocatable have the same effect
   allocate(a(n), temp(n))
   temp(1:size(a)) = a
end program
! { dg-final { scan-tree-dump-times "MAX_EXPR\[^\n\t\]+ubound\[^\n\t\]+lbound" 1 "original" } }
