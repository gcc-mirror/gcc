! { dg-do compile }
! { dg-options "-fimplicit-none" }
! PR fortran/78239 - used to ICE
program p
   character(*), parameter :: z(2) = [character(n) :: 'x', 'y'] ! { dg-error "Scalar INTEGER expression expected" }
end
