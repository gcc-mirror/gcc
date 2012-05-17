! { dg-do compile }
! PR 13575 -- we used to not see that c0 has no type, and then ICE later
module AHFinder_dat 
implicit none 
save c0 ! { dg-error "no IMPLICIT type" "no IMPLICIT type" }
end module AHFinder_dat
! PR 15978 -- we used to not see that aaa has no type, and then ICE later
implicit none
common/rommel/aaa ! { dg-error "no IMPLICIT type" "no IMPLICIT type" }
end
