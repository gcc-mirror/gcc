! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! Test that reduction optimization doesn't break with a function expression
! in an array constructor. 
program main
  implicit none
  integer, parameter :: dp=selected_real_kind(15)
  real(kind=dp), dimension(2,2) :: a
  real(kind=dp) thirteen

  data a /2._dp,3._dp,5._dp,7._dp/
  thirteen = 13._dp
  if (abs (product([[sum([eleven_ones()]), thirteen], a]) - 30030._dp) > 1e-8) call abort
 contains
  function eleven_ones()
    real(kind=dp) :: eleven_ones(11)
    integer       :: i

    eleven_ones = [ (1._dp, i=1,11) ]
  end function eleven_ones
end program main
! { dg-final { scan-tree-dump-times "while" 4 "original" } }

