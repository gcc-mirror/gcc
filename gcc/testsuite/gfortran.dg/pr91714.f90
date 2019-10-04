! { dg-do compile }
! Contributed by Gerhard Steinmetz
program p
   typea          ! { dg-error "Mangled derived type" }
      integer b
   end type       ! { dg-error "Expecting END PROGRAM" }
   type(a) :: c   ! { dg-error "is being used before it" }
   c = a(1)
   print *, c
end
