! { dg-do run }
! PR fortran/118120 - potential aliasing of complex pointer inquiry references
!
! Contributed by Slava Zakharin < szakharin at nvidia dot com >

program main
  implicit none
  integer :: k
  complex, target :: data(21)
  do k=1,21
     data(k) = cmplx(-k,0.0)
  end do
  call test(1, 1, data)
! print *, data
  if (     data(1)      /= -1.)           stop 1
  if (any (data(2:)% re /= [(k,k=1,20)])) stop 2
  call pr113928 ()
contains
  subroutine test(i, j, data)
    integer :: i, j
    complex, target  :: data(21)
    real, pointer    :: result(:,:,:,:)
    complex, pointer :: temp(:,:)
    result(i:i,j:j,1:4,1:5) => data(2:)%re
    temp(1:4,1:5)           => data(1:20)
    result(i,j,:,:) = abs(temp)
  end subroutine test
end program main

! PR fortran/113928
!
! Contributed by < eddyg_61-bugzilla at yahoo dot it >

subroutine pr113928
  implicit none
  integer, parameter :: N = 4
  complex, target    :: wz(N) = 0.
  real,    pointer   :: wr(:)
  integer :: i

  wr => wz%re
  wr = [(i,i=1,N)]
  wr = wr + wz(N:1:-1)%re
! print *, wr
  if (any (wr /= N+1)) stop 3
end
