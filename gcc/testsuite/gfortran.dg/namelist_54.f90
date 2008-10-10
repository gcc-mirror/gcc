! { dg-do run }
! PR37707 Namelist read of array of derived type incorrect.
type s
   integer m
   integer n
end type s
type(s) :: a(3)
character*80 :: l = ' &namlis a%m=1,2, a%n=5,6, /'
namelist /namlis/ a
a%m=[87,88,89]
a%n=[97,98,99]
read(l,namlis)
if (a(1)%m /= 1 .or. a(2)%m /= 2 .or. a(1)%n /= 5 .or. a(2)%n /= 6 .or. &
  & a(3)%m /= 89 .or. a(3)%n /= 99) call abort
end
