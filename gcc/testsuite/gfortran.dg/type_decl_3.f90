! { dg-do compile }
!
! PR fortran/39427
!
   subroutine t(x) ! { dg-error "conflicts with previously declared entity" }
     type(t) :: x ! { dg-error "conflicts with previously declared entity" }
   end subroutine t
