! { dg-do compile }
! PR 51260 - an unneeded parameter found its way into the
! assembly code. Original test case by Tobias Burnus.
module x
contains
  subroutine foo(i)
    integer, intent(in) :: i
  end subroutine foo
end module x

program main
  use x
  integer, parameter:: unneeded_parameter (10000)=(/(i,i=1,10000)/)
  call foo(unneeded_parameter (1))
  print *,unneeded_parameter (1)
end program
! { dg-final { scan-assembler-times "unneeded_parameter" 0 } }
