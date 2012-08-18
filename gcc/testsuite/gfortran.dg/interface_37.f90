! { dg-do compile }
!
! PR fortran/39290
! Subroutine/function ambiguity in generics.
!
     interface q
       subroutine qr(f)
         implicit real(f)
         external f
       end subroutine
       subroutine qc(f)
         implicit complex(f)
         external f
       end subroutine ! { dg-error "Ambiguous interfaces" }
     end interface q
   end
