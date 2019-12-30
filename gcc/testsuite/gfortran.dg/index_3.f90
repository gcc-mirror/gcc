! { dg-do run }
! PR 91651 - this used to give an ICE.
! Bug report by Gerhard Steinmetz.
program p
   integer :: z(2)
   z = index('100101', '10', [.false.,.true.],kind=4)
   if (z(1) /= 1 .or. z(2) /= 4) stop 1
end
