! { dg-do run }
module z
   integer :: i
   character(6) :: a(2) = (/ ('main  ' , i = 1, 2) /)
   character(6) :: b(2) = (/ 'abcd  ' , 'efghij' /)
end module

program y
  use z
  if (a(1) /= 'main  ') call abort
  if (a(2) /= 'main  ') call abort
  if (b(1) /= 'abcd  ') call abort
  if (b(2) /= 'efghij') call abort
end program y
