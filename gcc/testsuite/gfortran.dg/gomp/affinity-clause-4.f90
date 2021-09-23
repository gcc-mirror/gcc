subroutine foo
  integer :: A(10), B(10), C(10)
  interface
    integer function ibar(x)
      integer :: x
    end function ibar
  end interface

  !$omp parallel default(none)  ! { dg-note "enclosing 'parallel'" }
  !$omp task affinity (iterator(j=ibar(0):ibar(1):ibar(2)) : a(ibar(j)), b(j), c(j))
  !$omp end task
  !$omp end parallel
! { dg-error "'a' not specified in enclosing 'parallel'" "" { target *-*-* } .-3 }
! { dg-error "'b' not specified in enclosing 'parallel'" "" { target *-*-* } .-4 }
! { dg-error "'c' not specified in enclosing 'parallel'" "" { target *-*-* } .-5 }
end
