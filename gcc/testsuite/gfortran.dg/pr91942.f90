! { dg-do compile }
! PR fortran/91942
! Code contributed by Gerhard Steinmetz
program p
   integer :: i
   backspace (iostat=i%kind) ! { dg-error "Expecting a variable at" }
   endfile (iostat=i%kind) ! { dg-error "Expecting END PROGRAM" }
   flush (iostat=i%kind) ! { dg-error "Expecting a variable at" }
   rewind (iostat=i%kind) ! { dg-error "Expecting a variable at" }
end
