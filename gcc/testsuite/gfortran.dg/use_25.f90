! { dg-do compile }
!
! PR fortran/42769
! This test used to be rejected because the typebound call A%GET was
! simplified to MY_GET which is an ambiguous name in the main program
! namespace.
!
! Original testcase by Salvator Filippone <sfilippone@uniroma2.it>
! Reduced by Janus Weil <janus@gcc.gnu.org>

module mod1
  type :: t1
  contains
    procedure, nopass :: get => my_get
  end type
contains 
  subroutine my_get()
    print *,"my_get (mod1)"
  end subroutine
end module

module mod2
contains 
  subroutine my_get()    ! must have the same name as the function in mod1
    print *,"my_get (mod2)"
  end subroutine
end module

  use mod2
  use mod1
  type(t1) :: a
  call call_get
  contains
  subroutine call_get
    call a%get()
  end subroutine call_get
end


