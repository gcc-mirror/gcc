! { dg-do compile }

program p
   type t
      integer :: a
   end type
   type(t) :: x
   data x /t(z'1')/ ! { dg-error "BOZ" }
end

