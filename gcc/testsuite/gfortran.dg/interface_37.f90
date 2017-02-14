! { dg-do compile }
!
! PR fortran/39290
! Subroutine/function ambiguity in generics.
!
     interface q
       subroutine qr(f)  ! { dg-error "Ambiguous interfaces" }
         implicit real(f)
         external f
       end subroutine
       subroutine qc(f)  ! { dg-error "Ambiguous interfaces" }
         implicit complex(f)
         external f
       end subroutine
     end interface q
   end
