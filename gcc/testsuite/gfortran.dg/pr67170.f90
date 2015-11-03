! { dg-do compile }
! { dg-options "-O -fdump-tree-fre1" }

module test_module
  integer, parameter :: r=10
   integer :: data(r, r), block(r, r, r)
  contains
recursive subroutine foo(arg) 
integer, intent(in) :: arg
integer :: loop, x(r), y(r)

   where(data(arg, :) /= 0)
      x = data(arg, :)
       y = l
   elsewhere
      x = 1
      y = r
   end where

do loop = x(1), y(1)
   if(block(arg, 1, loop) <= 0) cycle
   block(arg, 1:4, loop) =  block(arg, 1:4, i1) + 1
   call foo(arg + 2)     
   block(arg, 1:4, loop) = block(arg, 1:4, i1) + 10
end do
end subroutine foo

end module test_module
end program

! { dg-final { scan-tree-dump-times "= \\*arg_\[0-9\]+\\(D\\);" 1 "fre1" } }
