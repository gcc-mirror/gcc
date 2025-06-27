!{ dg-do run }

! Contributed by  Federico Perini <federico.perini@gmail.com>
! Check that PR fortran/119106 is fixed.

program char_param_array
implicit none
character, parameter :: p(5) = ['1','2','3','4','5']
character, save      :: n(5) = ['1','2','3','4','5']
integer :: i(10), j

i = 4
if (any([(n(i(j)),j=1,10)] /= '4')) stop 1 ! OK
if (any([(p(i(j)),j=1,10)] /= '4')) stop 2 ! used to runtime out-of-bounds error

end program char_param_array

