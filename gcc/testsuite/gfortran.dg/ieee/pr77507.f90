! { dg-do compile }
Program p
  Use ieee_arithmetic
  Use iso_c_binding
  Print *, ieee_value(x=1.0, class=ieee_negative_inf)
  Print *, c_associated(c_ptr_1=c_null_ptr)
End Program
