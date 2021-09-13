! { dg-do run }
!
! Test the fix for PRs93924 & 93925.
!
! Contributed by Martin Stein  <mscfd@gmx.net>
!
module cs

implicit none

integer, target :: integer_target

abstract interface
   function classStar_map_ifc(x) result(y)
      class(*), pointer            :: y
      class(*), target, intent(in) :: x
   end function classStar_map_ifc
end interface

contains

   function fun(x) result(y)
      class(*), pointer            :: y
      class(*), target, intent(in) :: x
      select type (x)
      type is (integer)
         integer_target = x        ! Deals with dangling target.
         y => integer_target
      class default
         y => null()
      end select
   end function fun

   function apply(f, x) result(y)
      procedure(classStar_map_ifc) :: f
      integer, intent(in) :: x
      integer :: y
      class(*), pointer :: p
      y = 0                        ! Get rid of 'y' undefined warning
      p => f (x)
      select type (p)
      type is (integer)
         y = p
      end select
   end function apply

   function selector() result(f)
      procedure(classStar_map_ifc), pointer :: f
      f => fun
   end function selector

end module cs


program classStar_map

use cs
implicit none

integer :: x, y
procedure(classStar_map_ifc), pointer :: f

x = 123654
f => selector ()               ! Fixed by second chunk in patch
y = apply (f, x)               ! Fixed by first chunk in patch
if (x .ne. y) stop 1

x = 2 * x
y = apply (fun, x)             ! PR93925; fixed as above
if (x .ne. y) stop 2

end program classStar_map
