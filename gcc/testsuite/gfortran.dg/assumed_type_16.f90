! { dg-do compile }
! { dg-additional-options "-std=f2008" }
!
! PR fortran/104143
!
 interface
   subroutine foo(x)
     type(*) :: x(*)  ! { dg-error "Fortran 2018: Assumed type" }
   end
 end interface
 integer :: a
 call foo(a)  ! { dg-error "Type mismatch in argument" }
 call foo((a))  ! { dg-error "Type mismatch in argument" }
end
