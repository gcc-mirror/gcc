! PR fortran/77374
! { dg-do compile }

subroutine foo (a, b)
  integer :: a, b
!$omp atomic
  b = b + a
!$omp atomic
  z(1) = z(1) + 1	! { dg-error "must have the pointer attribute" }
end subroutine
subroutine bar (a, b)
  integer :: a, b
  interface
    function baz (i) result (res)
      integer, pointer :: res
      integer :: i
    end function
  end interface
!$omp atomic
  baz (i) = 1		! { dg-error "unexpected" }
end subroutine
