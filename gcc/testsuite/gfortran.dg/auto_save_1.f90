! { dg-do run }
! Check that automatic objects work properly in the presence of a save
! statement.
! PR21034
subroutine test(n)
  implicit none
  integer n
  real dte(n)
  character(len=n) :: s
  save
  dte = 0
  s = ""
end

program prog
  call test(4)
  call test(10)
end program
