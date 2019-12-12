! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_8.c }
!
! Test the fix for PR89842.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_alloc_01
  use, intrinsic :: iso_c_binding
  implicit none

  interface
     subroutine globalp(this) bind(c)
       import :: c_float
       real(c_float), allocatable :: this(:)
     end subroutine globalp
  end interface
end module mod_alloc_01

program alloc_01
  use mod_alloc_01
  implicit none

  real(c_float), allocatable :: myp(:) 
  integer :: status

  status = 0
  call globalp(myp)

!  write(*,*) 'globalp done'
  if (.not. allocated(myp)) then
     write(*,*) 'FAIL 1'
     stop 1
  end if
  if (lbound(myp,1) /= 3 .or. size(myp,1) /= 4) then
     write(*,*) 'FAIL 2: ', lbound(myp), size(myp,1)
     status = status + 1
  else
!     write(*,*) 'Now checking data', myp(3)
     if (maxval(abs(myp - [1.1, 2.3, 5.1, 4.2])) > 1.0e-6) then
        write(*,*) 'FAIL 3: ', myp
        status = status + 1
     end if
  end if

  if (status .ne. 0) then
     stop status
  end if
end program alloc_01

