! { dg-do compile }
!
! PR fortran/18918
!
! Check for error if no -fcoarray= option has been given
!

integer :: a
integer :: b[*] ! { dg-error "Coarrays disabled" }

error stop "Error"
sync all ! { dg-error "Coarrays disabled" }

critical ! { dg-error "Coarrays disabled" }
end critical ! { dg-error "Expecting END PROGRAM statement" }

end
