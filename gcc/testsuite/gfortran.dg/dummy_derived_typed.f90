! { dg-do compile }
subroutine s(t)   ! { dg-error "Dummy argument" }
   type t         ! { dg-error "cannot be a derived" }
   end type       ! { dg-error "Expecting END SUBROUTINE" }
end
