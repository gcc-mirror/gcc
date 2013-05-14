! { dg-do compile }
use iso_c_binding
implicit none
character(kind=c_char,len=256),target :: arg
type(c_ptr),pointer :: c
c = c_loc(arg) ! OK since Fortran 2003, Tech Corrigenda 5; IR F03/0129

end
