! { dg-do compile }
! { dg-options "-O1" }
! { dg-require-visibility "" }
!
! PR fortran/54221
!
! Check that the unused PRIVATE "aaaa" variable is optimized away
!

module m
  private
  integer, save :: aaaa
end module m

! The xfail below has appeared with the introduction of submodules. 'aaaa'
! now is TREE_PUBLIC but has DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN set.

! { dg-final { scan-assembler-not "aaaa" { xfail *-*-* } } }
