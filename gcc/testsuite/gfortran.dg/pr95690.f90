! { dg-do compile }
module m
contains
   subroutine s
      print *, (erfc) ! { dg-error "not a floating constant" "" { target i?86-*-* x86_64-*-* *-*-solaris* } }
   end ! { dg-error "not a floating constant" "" { target { ! "i?86-*-* x86_64-*-* *-*-solaris*" } } }
   function erfc()
   end
end
