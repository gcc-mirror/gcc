! { dg-do compile }
!
! PR fortran/57549
!
! Contributed by Vladimir Fuka
!
 type t
 end type
 type(t),allocatable :: a(:)
 a = [t::t()]
 print *, [ integer :: ]
end

subroutine invalid()
    print *, [ type(integer) :: ] ! { dg-error "Syntax error in array constructor" }
    print *, [ type(tt) :: ]      ! { dg-error "Syntax error in array constructor" }
end subroutine invalid
