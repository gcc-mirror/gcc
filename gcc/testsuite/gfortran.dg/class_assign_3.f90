! { dg-do link }
!
! PR 84543: undefined reference to __copy_INTEGER_4_.3788
!
! Contributed by Neil Carlson <neil.n.carlson@gmail.com>

class(*), allocatable :: x
x = 42
end
