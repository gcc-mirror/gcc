! { dg-do compile }
! { dg-options "" }
!
! PR fortran/50405
!
! Submitted by zeccav@gmail.com
!
       f(f) = 0 ! { dg-error "Self-referential argument" }
       end
