! { dg-do compile }
program p
   backspace (err=1) ! { dg-error "UNIT number missing" }
   endfile (err=1)   ! { dg-error "UNIT number missing" }
   flush (err=1)     ! { dg-error "UNIT number missing" }
   rewind (err=1)    ! { dg-error "UNIT number missing" }
end
