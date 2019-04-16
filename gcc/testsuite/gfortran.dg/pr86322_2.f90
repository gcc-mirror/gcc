! { dg-do compile }
program bar
   type a
     integer :: i
   end type a
   type b
     type(a),pointer :: j
   end type b
   integer, target, save :: k = 42
   type(b) x
   data x%j%i/k/  ! { dg-error "is not rightmost part-ref" }
   print *, x%j%i
end program bar
