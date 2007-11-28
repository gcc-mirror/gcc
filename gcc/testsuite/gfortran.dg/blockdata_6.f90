! { dg-do compile }
! { dg-options "-std=f95" }
! PR34227 Initialized symbol in COMMON: Missing checks
program main
 implicit none
 integer, parameter:: nmin = 2
 character(len=3) :: emname(nmin)
 data emname/'bar','baz'/
 common/dd/emname ! { dg-error "can only be COMMON in BLOCK DATA" }
end program main
