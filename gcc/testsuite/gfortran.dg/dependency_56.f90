! { dg-do compile }
! PR 91783 - used to cause an ICE in dependency checking.
! Test case by Gerhard Steinmetz.
program p
   class(*), allocatable :: a(:)
   a = [1, 2, 3]
   a = f(a)
contains
   function f(x) result(y)
      class(*), allocatable, intent(in) :: x(:)
      class(*), allocatable :: y(:)
      y = x
   end
end
