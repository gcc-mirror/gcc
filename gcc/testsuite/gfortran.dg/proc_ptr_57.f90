! { dg-do compile }
! { dg-additional-options "-fcheck=pointer" }
!
! PR fortran/102900

module cs
  implicit none
  interface
     function classStar_map_ifc() result(y)
       import
       class(*), pointer :: y
     end function classStar_map_ifc
  end interface

contains

   function selector()
     procedure(classStar_map_ifc), pointer :: selector
     selector => NULL()
   end function selector

   function selector_result() result(f)
     procedure(classStar_map_ifc), pointer :: f
     f => NULL()
   end function selector_result

   function fun(x) result(y)
      class(*), pointer            :: y
      class(*), target, intent(in) :: x
      select type (x)
      class default
         y => null()
      end select
   end function fun

end module cs
