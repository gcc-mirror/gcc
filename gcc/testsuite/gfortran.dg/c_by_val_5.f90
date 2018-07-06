! { dg-do run }
! Overwrite -pedantic setting:
! { dg-options "-Wall" }
!
! Tests the fix for PR31668, in which %VAL was rejected for
! module and internal procedures.
! 

subroutine bmp_write(nx)
  implicit none
  integer, value :: nx
  if(nx /= 10) STOP 1
  nx = 11
  if(nx /= 11) STOP 2
end subroutine bmp_write

module x
 implicit none
 ! The following interface does in principle
 ! not match the procedure (missing VALUE attribute)
 ! However, this occures in real-world code calling
 ! C routines where an interface is better than
 ! "external" only.
 interface
   subroutine bmp_write(nx)
     integer, value :: nx
   end subroutine bmp_write
 end interface
contains
   SUBROUTINE Grid2BMP(NX)
     INTEGER, INTENT(IN) :: NX
     if(nx /= 10) STOP 3
     call bmp_write(%val(nx))
     if(nx /= 10) STOP 4
   END SUBROUTINE Grid2BMP
END module x

! The following test is possible and
! accepted by other compilers, but
! does not make much sense.
! Either one uses VALUE then %VAL is
! not needed or the function will give
! wrong results.
!
!subroutine test()
!    implicit none
!    integer :: n
!    n = 5
!    if(n /= 5) STOP 5
!    call test2(%VAL(n))
!    if(n /= 5) STOP 6
!  contains
!    subroutine test2(a)
!      integer, value :: a
!      if(a /= 5) STOP 7
!      a = 2
!      if(a /= 2) STOP 8
!    end subroutine test2
!end subroutine test

program main
  use x
  implicit none
!  external test
  call Grid2BMP(10)
!  call test()
end program main
