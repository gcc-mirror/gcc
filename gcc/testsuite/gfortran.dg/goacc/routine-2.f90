! { dg-do compile }

  module m1
    contains
    recursive function mfact (x) result (res)
      integer, intent(in) :: x
      integer :: res
      integer i
      i = 0
      !$acc routine  ! { dg-error "Unexpected \\\!\\\$ACC ROUTINE" }
      if (x < 1) then
         res = 1
      else
         res = x * mfact (x - 1)
      end if
    end function mfact
  end module m1
