! { dg-do compile }
! Test the fix for PR43492, in which the generic call caused and ICE.
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>
!
module base_mod
  
  type  :: base_mat
    integer, private     :: m, n
  contains 
    procedure, pass(a) :: transp1 => base_transp1
    generic, public    :: transp => transp1
    procedure, pass(a) :: transc1 => base_transc1
    generic, public    :: transc => transc1
  end type base_mat

contains

  subroutine base_transp1(a)
    implicit none 
    
    class(base_mat), intent(inout) :: a
    integer :: itmp
    itmp        = a%m
    a%m         = a%n
    a%n         = itmp
  end subroutine base_transp1
  subroutine base_transc1(a)
    implicit none 
    class(base_mat), intent(inout) :: a
    
    call a%transp() 
!!$    call a%transp1() 
  end subroutine base_transc1


end module base_mod
! { dg-final { cleanup-modules "m" } }
