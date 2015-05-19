! { dg-do compile }
! PR fortran/66039
!
! Original code from Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
subroutine p1
   rewind ((     ! { dg-error "Syntax error in REWIND" }
   rewind (-     ! { dg-error "Syntax error in REWIND" }
end subroutine p1

subroutine p2
   flush ((      ! { dg-error "Syntax error in FLUSH" }
   flush (-      ! { dg-error "Syntax error in FLUSH" }
end subroutine p2

subroutine p4
   backspace ((   ! { dg-error "Syntax error in BACKSPACE" }
   backspace (-   ! { dg-error "Syntax error in BACKSPACE" }
end subroutine p4

subroutine p3
   endfile ((     ! { dg-error "Expecting END SUBROUTINE" }
   endfile (-     ! { dg-error "Expecting END SUBROUTINE" }
end subroutine p3

