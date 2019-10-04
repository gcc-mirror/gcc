! { dg-do run }
! { dg-options "-fdec-static -frecursive" }

! Contributed by Mark Eggleston <mark.eggleston@codethink.com>
!
! Check automatic variables can be used in equivalence statements.
! Any other variables that do not explicitly have the automatic
! attribute will be given the automatic attribute.
!
! Check that variables are on the stack by incorporating the
! equivalence in a recursive function.
!
program test
  integer :: f

  f = factorial(5)
  if (f.ne.120) stop 2

contains
  function factorial(n) result(f)
    integer :: f
    integer, intent(in) :: n
    integer, automatic :: a
    integer :: b
    equivalence (a,b)

    if (loc(a).ne.loc(b)) stop 1
    b = n
    if (a.eq.1) then
      f = 1
    else
      f = a * factorial(b-1)
    end if
  end function
end program test
