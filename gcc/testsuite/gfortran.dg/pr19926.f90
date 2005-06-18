! { dg-do run }
module b
  type cat
     integer :: i = 0
  end type cat
end module b

program a
  use b
  type(cat) z
  integer :: i = 0, j(4,3,2) = 0
  call string_comp(i)
  if (i /= 3) call abort
  call string_comp(z%i)
  if (z%i /= 3) call abort
  call string_comp(j(1,2,1))
  if (j(1,2,1) /= 3) call abort
end program a

subroutine string_comp(i)
   integer, parameter :: map(0:50) = 3
   integer :: i
   i = map(42)
end subroutine string_comp

