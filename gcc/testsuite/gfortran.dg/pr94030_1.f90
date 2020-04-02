! { dg-do compile }
!

subroutine f(n)
  integer :: n
  integer :: arr(n)
  integer :: i
  equivalence (i, arr(1))
end

! { dg-error "Array 'arr' at .1. with non-constant bounds cannot be an EQUIVALENCE object" " " { target *-*-* } 8 }
