! { dg-do compile }
! PR fortran/26551
 function func2()
   integer func2
   func2 = 42
   return
 entry c() result (foo)
   foo = barbar()
   return
 entry b() result (bar)
   bar = 12
   return
 contains
   function barbar ()
     barbar = b () ! { dg-error "is not declared as RECURSIVE" }
   end function barbar
 end function
