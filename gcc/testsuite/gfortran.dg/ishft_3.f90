! { dg-do compile }
! PR fortran/50514
program ishft_3

   implicit none

   integer j, m

   m = 42
   !
   ! These should compile.
   !
   j = ishft(m, 16)
   j = ishft(m, -16)
   j = ishftc(m, 16)
   j = ishftc(m, -16)
   !
   ! These should issue an error.
   !
   j = ishft(m, 640)    ! { dg-error "absolute value of SHIFT" }
   j = ishftc(m, 640)   ! { dg-error "absolute value of SHIFT" }
   j = ishft(m, -640)   ! { dg-error "absolute value of SHIFT" }
   j = ishftc(m, -640)  ! { dg-error "absolute value of SHIFT" }

   ! abs(SHIFT) must be <= SIZE

   j = ishftc(m,  1, 2)
   j = ishftc(m,  1, 2)
   j = ishftc(m, -1, 2)
   j = ishftc(m, -1, 2)

   j = ishftc(m,  10, 2)! { dg-error "absolute value of SHIFT" }
   j = ishftc(m,  10, 2)! { dg-error "absolute value of SHIFT" }
   j = ishftc(m, -10, 2)! { dg-error "absolute value of SHIFT" }
   j = ishftc(m, -10, 2)! { dg-error "absolute value of SHIFT" }

   j = ishftc(m, 1, -2) ! { dg-error "must be positive" }
end program
