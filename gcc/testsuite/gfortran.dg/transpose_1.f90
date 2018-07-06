! { dg-do compile }
! Tests the fix for PR32962, in which the result of TRANSPOSE, when
! an actual argument of an elemental intrinsic would receive the
! wrong offset.
!
! Contributed by Wirawan Purwanto <wirawan0@gmail.com>
!
  real(kind=8), allocatable :: b(:,:)
  real(kind=8) :: a(2,2), c(2,2)
  i = 2
  allocate (b(i,i))
  a(1,1) = 2
  a(2,1) = 3
  a(1,2) = 7
  a(2,2) = 11
  call foo
  call bar
  if (any (c .ne. b)) STOP 1
contains
  subroutine foo
    b = cos(transpose(a))
  end subroutine
  subroutine bar
    c = transpose(a)
    c = cos(c)
  end subroutine
end program
