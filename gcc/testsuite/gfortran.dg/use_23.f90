! { dg-do compile }
!
! PR fortran/42769
! This test used to ICE in resolve_typebound_procedure because T1's GET
! procedure was wrongly associated to MOD2's MY_GET (instead of the original
! MOD1's MY_GET) in MOD3's SUB.
!
! Original testcase by Salvator Filippone <sfilippone@uniroma2.it>
! Reduced by Janus Weil <janus@gcc.gnu.org>

module mod1
  type :: t1
  contains
    procedure, nopass :: get => my_get
  end type
contains 
  logical function my_get()
  end function
end module

module mod2
contains 
  logical function my_get()
  end function
end module

module mod3
contains
  subroutine sub(a)
    use mod2, only: my_get
    use mod1, only: t1
    type(t1) :: a
  end subroutine
end module


use mod2, only: my_get
use mod3, only: sub
end 



