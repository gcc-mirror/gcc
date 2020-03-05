! { dg-do run }
!
! Test the fix for PR90786.
!
! Contributed by Andrew benson  <abensonca@gmail.com>
!
module f
procedure(c), pointer :: c_

 type :: s
   integer :: i = 42
 end type s
 class(s), pointer :: res, tgt

contains

 function c()
   implicit none
   class(s), pointer ::  c
   c => tgt
   return
 end function c

 subroutine fs()
   implicit none
   c_ => c  ! This used to ICE
   return
 end subroutine fs

end module f

  use f
  allocate (tgt, source = s(99))
  call fs()
  res => c_()
  if (res%i .ne. 99) stop 1
  deallocate (tgt)
end
