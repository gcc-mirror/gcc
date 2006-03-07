! { dg-do compile }
! Tests the fix for PR24557 in which the return of a
! temporary character(*) array would cause an ICE.
!
! Test case provided by Erik Edelmann  <eedelmann@gcc.gnu.org>
!
  character(4) :: a(2)
  print *, fun (a)
contains
  function fun (arg)
    character (*) :: arg (10)
    integer :: fun(size(arg))
    fun = 1
  end function fun
end
