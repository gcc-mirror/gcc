! { dg-do compile }
module m
contains
   subroutine s
      print *, (erfc)
   end
   function erfc()
   end
end
! The actual line number is target dependent, allow any
! { dg-error "not a floating constant" "" { target *-*-* } 0 }
