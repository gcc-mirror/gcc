! { dg-do compile }
!
! PR fortran/18918
!
! Check for error if no -fcoarray= option has been given
!

integer :: a
integer :: b[*] ! { dg-error "Coarrays disabled" }

error stop "Error"
sync all !  "Coarrays disabled"  (but error above is fatal)

critical ! "Coarrays disabled"  (but error above is fatal)

end critical ! "Expecting END PROGRAM statement"  (but error above is fatal)

end
