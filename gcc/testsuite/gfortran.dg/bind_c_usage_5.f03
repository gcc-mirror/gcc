! { dg-do compile }
module bind_c_usage_5
use, intrinsic :: iso_c_binding

bind(c) c3, c4 
integer(c_int), bind(c) :: c3 ! { dg-error "Duplicate BIND attribute" }
integer(c_int) :: c4
end module bind_c_usage_5
