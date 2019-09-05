! { dg-do compile }
! PR fortran/91660
program foo
   type(doubleprecision :: x   ! { dg-error "Malformed type-spec" }
   type(double precision :: y  ! { dg-error "Malformed type-spec" }
   type(character(len=3) :: a  ! { dg-error "Malformed type-spec" }
   type(doublecomplex :: b     ! { dg-error "Malformed type-spec" }
   type(double complex :: c    ! { dg-error "Malformed type-spec" }
end program foo
