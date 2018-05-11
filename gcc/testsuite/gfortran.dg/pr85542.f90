! { dg-do compile }
! PR fortran/85542
function f(x)
   character(*), intent(in) :: x
   character((len((x)))) :: f
   f = x
end
