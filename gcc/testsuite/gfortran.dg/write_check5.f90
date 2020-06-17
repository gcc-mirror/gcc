! { dg-do compile }
!
! The asynchronous specifier for a data transfer statement shall be
! an initialization expression
!

module write_check5
contains

function no()
  implicit none
  character(3) :: no
  no = "yes"
endfunction

end module

use write_check5
implicit none

open (unit=10, asynchronous=no())              ! Ok, it isn't a transfer stmt
write(*,*, asynchronous=no())  ! { dg-error "must be an intrinsic function" }
read (*,*, asynchronous=no())  ! { dg-error "must be an intrinsic function" }
end
