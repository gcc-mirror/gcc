! { dg-do compile }
! { dg-options "-O3 -ffast-math -fdump-tree-optimized -fno-protect-parens" }
!
! PR fortran/35259
! Test for -fno-protect-parens
!
function test(b)
  real a
  a = (b + 5.) - 5.
  test = a
end

! Test copied from reassoc_1.f90 which checked for -fprotect-parens (default),
! and thus for the occurance of "5 - 5".
!
! We need an explicit +5 and -5, and an intermediate ((bla)) expression
! (the reassoc barrier).  Make use of "." matching lineends.
! { dg-final { scan-tree-dump-times "\\\+ 5.*\\\)\\\).* - 5" 0 "optimized" } }
