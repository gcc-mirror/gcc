! { dg-do compile }
! { dg-options "-std=f95" }
! Test for PROCEDURE statements with the -std=f95 flag.
! Contributed by Janus Weil <jaydub66@gmail.com>

program p

procedure():: proc  ! { dg-error "Fortran 2003: PROCEDURE statement" }

end program 
