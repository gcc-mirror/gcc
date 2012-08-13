! { dg-do compile }
! { dg-options "-O1" }
!
! PR fortran/54221
!
! Check that the unused PRIVATE "aaaa" variable is optimized away
!

module m
  private
  integer, save :: aaaa
end module m

! { dg-final { scan-assembler-not "aaaa" } }
