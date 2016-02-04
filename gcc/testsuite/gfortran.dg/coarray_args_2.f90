! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Check argument passing.
! Taken from Reinhold Bader's fortran_tests.
! 

module mod_rank_mismatch_02
  implicit none
  integer, parameter :: ndim = 2
contains
  subroutine subr(n,w)
    integer :: n
    real :: w(n,*)[*] 

    integer :: k, x

    if (this_image() == 0) then
       x = 1.0
       do k = 1, num_images() 
           if (abs(w(2,1)[k] - x) > 1.0e-5) then
              write(*, *) 'FAIL'
              error stop
           end if
           x = x + 1.0
       end do
    end if

  end subroutine
end module

program rank_mismatch_02
  use mod_rank_mismatch_02
  implicit none
  real :: a(ndim,2)[*]

  a = 0.0
  a(2,2) = 1.0 * this_image() 

  sync all

  call subr(ndim, a(1:1,2)) ! OK
  call subr(ndim, a(1,2)) ! See also F08/0048 and PR 45859 about the validity
  if (this_image() == 1) then
     write(*, *) 'OK'
  end if
end program
