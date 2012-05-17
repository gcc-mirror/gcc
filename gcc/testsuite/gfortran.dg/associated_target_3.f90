! { dg-do run }
!
! PR fortran/41777
!
module m
type t2
 integer :: i
end type t2
interface f
 module procedure f2
end interface f
contains
function f2(a)
  type(t2), pointer :: f2,a
  f2 => a
end function f2
end module m

use m
implicit none
type(t2), pointer :: a
allocate(a)
if (.not. associated(a,f(a))) call abort()
call cmpPtr(a,f2(a))
call cmpPtr(a,f(a))
deallocate(a)
contains
  subroutine cmpPtr(a,b)
    type(t2), pointer :: a,b
!    print *, associated(a,b)
    if (.not. associated (a, b)) call abort()
  end subroutine cmpPtr
end
