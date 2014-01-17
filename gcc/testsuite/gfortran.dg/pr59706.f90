! PR middle-end/59706
! { dg-do compile }

  integer i
  do concurrent (i=1:2)
  end do
contains
  subroutine foo
  end 
end
