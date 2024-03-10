! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR96624 in which an attempt was made to assign
! to the zero length temporary created by reshape, resulting in a segfault.
!
! Contributed by Dong Shenpo  <shenpo.dong@compiler-dev.com>
!
program test
  integer :: a(2,0)
  a = reshape([1,2,3,4], [2,0])
  print *, a
end
! { dg-final { scan-tree-dump-not "data..0. =" "original" } }
