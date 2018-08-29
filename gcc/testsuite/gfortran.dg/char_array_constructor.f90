! { dg-do run }
module z
   integer :: i
   character(6) :: a(2) = (/ ('main  ' , i = 1, 2) /)
   character(6) :: b(2) = (/ 'abcd  ' , 'efghij' /)
end module

program y
  use z
  if (a(1) /= 'main  ') STOP 1
  if (a(2) /= 'main  ') STOP 2
  if (b(1) /= 'abcd  ') STOP 3
  if (b(2) /= 'efghij') STOP 4
end program y
