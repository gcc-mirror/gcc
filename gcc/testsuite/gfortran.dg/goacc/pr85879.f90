! PR middle-end/85879
! { dg-do compile }

program p
   integer, pointer :: i
   integer, target :: j
   j = 2
   i => j
   !$acc parallel
   j = i
   !$acc end parallel
end
