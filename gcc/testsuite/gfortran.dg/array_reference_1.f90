! { dg-do run }
! Tests the fix for PR31994, aka 31867, in which the offset
! of 'a' in both subroutines was being evaluated incorrectly.
! The testcase for PR31867 is char_length_5.f90
!
! Contributed by Elizabeth Yip <elizabeth.l.yip@boeing.com>
!            and Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
program main
  call PR31994
  call PR31994_comment6
contains
  subroutine PR31994
    implicit none
    complex (kind=4), dimension(2,2) :: a, b, c
    a(1,1) = (1.,1.)
    a(2,1) = (2.,2.)
    a(1,2) = (3.,3.)
    a(2,2) = (4.,4.)
    b=conjg (transpose (a))
    c=transpose (a)
    c=conjg (c)
    if (any (b .ne. c)) STOP 1
  end subroutine PR31994
  subroutine PR31994_comment6
    implicit none
    real ,dimension(2,2)::a
    integer ,dimension(2,2) :: b, c
    a = reshape ((/1.,2.,3.,4./), (/2,2/))
    b=int (transpose(a))
    c = int (a)
    c = transpose (c)
    if (any (b .ne. c)) STOP 2
  end subroutine PR31994_comment6
END program main
