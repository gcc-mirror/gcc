!{ dg-do run }
!
! Check pointer aliasing is done w/o temp.
! Contributed by Arseny Solokha  <asolokha@gmx.com>

program pr107143
  type ta
     integer, POINTER :: ip(:)
  end type ta

  type tb
     integer, POINTER :: ip(:,:)
  end type tb

  integer, parameter :: cnt = 3
  type(ta) :: a(cnt)
  type(tb) :: b(cnt)
  integer, target :: arr(8) = [1,2,3,4,5,6,7,8]

  do i = 1, cnt
    allocate(a(i)%ip(8), SOURCE=arr * i)
  end do
  call s5(b, a, 2, 4)

  do i = 1, cnt
    if (any(b(i)%ip /= reshape(arr * i, [2, 4]))) stop i
  end do

contains

subroutine s5(y,z,n1,n2)

  type(tb) :: y(:)
  type(ta), TARGET :: z(:)

  forall (i=1:cnt)
    y(i)%ip(1:n1,1:n2) => z(i)%ip
  end forall
end subroutine s5
end program
