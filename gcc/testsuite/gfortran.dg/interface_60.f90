! { dg-do run }
! { dg-options "-Wexternal-argument-mismatch" }
! Originally proc_ptr_52.f90, this gave an error with the warning above.

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

   function apply(fap, x) result(y)
      procedure(classStar_map_ifc) :: fap
      integer, intent(in) :: x
      integer :: y
      class(*), pointer :: p
      y = 0                        ! Get rid of 'y' undefined warning
      p => fap (x)
      select type (p)
      type is (integer)
         y = p
      end select
   end function apply

   function selector() result(fsel)
      procedure(classStar_map_ifc), pointer :: fsel
      fsel => fun
   end function selector

end module cs


program classStar_map

use cs
implicit none

integer :: x, y
procedure(classStar_map_ifc), pointer :: fm

x = 123654
fm => selector ()               ! Fixed by second chunk in patch
y = apply (fm, x)               ! Fixed by first chunk in patch
if (x .ne. y) stop 1

x = 2 * x
y = apply (fun, x)             ! PR93925; fixed as above
if (x .ne. y) stop 2

end program classStar_map
