! { dg-do compile }
! { dg-options "-O0" }
! PR20899 - Common block variables cannot be equivalenced in a pure procedure.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
common /z/ i
contains
pure integer function test(j)
  integer, intent(in) :: j
  common /z/ i
  integer :: k
  equivalence(i,k) ! { dg-error "EQUIVALENCE object in the pure" }
  k=1 ! { dg-error "in PURE procedure at" }
  test=i*j
end function test
end

