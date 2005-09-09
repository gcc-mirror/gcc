! { dg-do run }
! This program tests that equivalence only associates variables in
! the same scope.
!
! provided by Paul Thomas - pault@gcc.gnu.org
!
program contained_equiv
  real a
  a = 1.0
  call foo ()
  if (a.ne.1.0) call abort ()
contains
  subroutine foo ()
    real b
    equivalence (a, b)
    b = 2.0
  end subroutine foo
end program contained_equiv

