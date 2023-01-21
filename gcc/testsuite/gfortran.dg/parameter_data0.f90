! { dg-do compile }
! PR fortran/102595  Similar to 88048 with a zero sized array
program p
   complex, parameter:: x(0) = 2
   data x%im /3.0/ ! { dg-error "shall not appear in a DATA statement" }
end
