! { dg-do compile }
program p
   use ieee_arithmetic
   real(kind=ieee_selected_real_kind(10_1)) :: z1
   real(kind=ieee_selected_real_kind(10_2)) :: z2
   real(kind=ieee_selected_real_kind(10_4)) :: z4
end
