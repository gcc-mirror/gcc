! { dg-do compile }
! { dg-options "-std=gnu" }
!
real :: a(4), b(4)
real :: c
integer :: istat, j
character(25) :: msg

open(10, file='mydata_f2003_io_8', asynchronous="yes", blank="null")
write(10,'(10f8.3)', asynchronous='no', decimal="comma", id=j) a ! { dg-error "must be with ASYNCHRONOUS=" }
read(10,'(10f8.3)', id=j, decimal="comma", blank="zero") b ! { dg-error "must be with ASYNCHRONOUS=" }
read(10,'(10f8.3)', asynchronous=msg, decimal="comma", blank="zero") b ! { dg-error "does not reduce to a constant expression" }
end
