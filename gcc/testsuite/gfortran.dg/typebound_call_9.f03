! { dg-do compile }

! PR fortran/37638
! If a PASS(arg) is invalid, a call to this routine later would ICE in
! resolving.  Check that this also works for GENERIC, in addition to the
! PR's original test.

! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module foo_mod
  implicit none 

  type base_foo_type 
    integer           :: nr,nc
    integer, allocatable :: iv1(:), iv2(:)

  contains

    procedure, pass(a) :: makenull ! { dg-error "has no argument 'a'" }
    generic :: null2 => makenull   ! { dg-error "Undefined specific binding" }

  end type base_foo_type

contains

  subroutine makenull(m)
    implicit none
    type(base_foo_type), intent(inout) :: m

    m%nr=0
    m%nc=0

  end subroutine makenull

  subroutine foo_free(a,info)
    implicit none
    Type(base_foo_type), intent(inout)  :: A
    Integer, intent(out)        :: info
    integer             :: iret
    info  = 0


    if (allocated(a%iv1)) then
      deallocate(a%iv1,stat=iret)
      if (iret /= 0) info = max(info,2)
    endif
    if (allocated(a%iv2)) then
      deallocate(a%iv2,stat=iret)
      if (iret /= 0) info = max(info,3)
    endif

    call a%makenull()
    call a%null2 () ! { dg-error "should be a SUBROUTINE" }

    Return
  End Subroutine foo_free

end module foo_mod

! { dg-final { cleanup-modules "foo_mod" } }
