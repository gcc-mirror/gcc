! { dg-do compile }
use iso_c_binding
implicit none
character(kind=c_char,len=256),target :: arg
type(c_ptr),pointer :: c
c = c_loc(arg) ! { dg-error "must have a length of 1" }

end
