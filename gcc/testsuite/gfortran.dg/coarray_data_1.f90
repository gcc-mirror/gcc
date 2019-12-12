! { dg-do  run }
! { dg-options "-fcoarray=lib -lcaf_single " }
! { dg-additional-options "-latomic" { target libatomic_available } }
! PR 71066 - this used to ICE
program p
   real :: a(2,2)[*]
   integer :: b(2,2)[*]
   data a /4*0.0/
   data b /1234, 2345, 3456, 4567/
   if (any (a /= 0.0)) stop 1
   if (any (b /= reshape([1234, 2345, 3456, 4567],[2,2]))) stop 2
end
