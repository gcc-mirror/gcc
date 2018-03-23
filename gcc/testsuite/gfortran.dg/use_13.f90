! { dg-do run }
!
! PR fortran/44360
!
! Test-case based on a contribution of Vittorio Zecca.
!
! The used subroutine was not the use-associated but the host associated one!
! The use-associated function/variable were already working properly.
!
module m
  integer :: var = 43
contains
 integer function fun()
   fun = 42
 end function fun
 subroutine fun2()
   var = 44
 end subroutine fun2
end module m

module m2
  integer :: var = -2
contains
 subroutine test()
   ! All procedures/variables below refer to the ones in module "m"
   ! and not to the siblings in this module "m2".
   use m 
   if (fun() /= 42) STOP 1
   if (var /= 43) STOP 2
   call fun2()
   if (var /= 44) STOP 3
 end subroutine test
 integer function fun()
   STOP 4
   fun = -3
 end function fun
 subroutine fun2()
   STOP 5
 end subroutine fun2
end module m2

use m2
call test()
end
