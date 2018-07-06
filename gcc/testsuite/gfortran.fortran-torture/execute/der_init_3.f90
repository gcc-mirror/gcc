! PR15365
! Default initializers were being missed
program main
  type xyz
     integer :: x = 123
  end type xyz

  type (xyz) :: a  !! ok
  type (xyz) b    !!! not initialized !!!
  if (a%x.ne.123) STOP 1
  if (b%x.ne.123) STOP 2
end
