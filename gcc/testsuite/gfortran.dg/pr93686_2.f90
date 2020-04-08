! { dg-do compile }
! PR fortran/93686

program p
   type t
      integer :: a = 1
   end type
   type(t), pointer :: x
   data x /t(2)/ ! { dg-error "part-ref with pointer attribute near ... is not rightmost part-ref of data-stmt-object" }
end
