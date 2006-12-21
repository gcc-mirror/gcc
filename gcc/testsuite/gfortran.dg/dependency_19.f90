! { dg-do compile }
! Tests the fix for PR30273, in which the pointer assignment was
! wrongly determined to have dependence because NULL() was not
! recognised by the analysis.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
module gfcbug49
  implicit none

  type spot_t
     integer, pointer     :: vm(:,:,:)
  end type spot_t

  type rc_t
    integer               :: n
    type(spot_t), pointer :: spots(:) => NULL()
  end type rc_t  

contains

  subroutine construct (rc, n)
    type(rc_t), intent(out) :: rc
    integer   , intent(in)  :: n
    integer :: k
    rc% n = n
    allocate (rc% spots (n))
    forall (k=1:n)
       rc% spots (k)% vm => NULL() ! gfortran didn't swallow this
    end forall
  end subroutine construct

end module gfcbug49
! { dg-final { cleanup-modules "gfcbug49" } }
