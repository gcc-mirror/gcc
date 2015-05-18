! { dg-do compile }
! PR fortran/66106
!
! Original code from Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
program p
   interface operator ( .gt. )
   end interface operator        ! { dg-error "END INTERFACE OPERATOR" }
end program p                    ! { dg-error "END INTERFACE" }
! { dg-excess-errors "Unexpected end of file" }
