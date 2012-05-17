! { dg-do compile }
! Tests the fix for a further regression caused by the
! fix for PR28788 and posted as PR28908. The problem was
! caused by the patch preventing interface derived types
! from associating with identical derived types in the
! containing namespaces.
!
! Contributed by HJ Lu  <hjl@lucon.org>
!
module bar
  implicit none
  public
  type ESMF_Time
  sequence
    integer :: MM
  end type
  public operator (+)
  private add
  interface operator (+)
  module procedure add
  end interface
contains
    function add (x, y)
      type(ESMF_Time) :: add
      type(ESMF_Time), intent(in) :: x
      type(ESMF_Time), intent(in) :: y
      add = x
    end function add
end module bar

module foo
  use bar
  implicit none
  private
  type ESMF_Clock
  sequence
    type(ESMF_Time)  :: CurrTime
  end type
contains
  subroutine ESMF_ClockAdvance(clock)
  use bar
    type(ESMF_Clock), intent(inout) :: clock
    clock%CurrTime = clock%CurrTime + clock%CurrTime
  end subroutine ESMF_ClockAdvance
end module foo
