! { dg-do compile }
!
! PR 86545: ICE in transfer_expr on invalid WRITE statement
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

   type tString
      character(len=:), allocatable :: cs
   end type

   interface my_trim
      module procedure trim_string
   end interface

contains

   elemental function trim_string(self) result(str)
      type(tString) :: str
      class(tString), intent(in) :: self
   end function

end module


program p
   use m
   type(tString) :: s
   write(*,*) my_trim(s)   ! { dg-error "cannot have ALLOCATABLE components" }
end
