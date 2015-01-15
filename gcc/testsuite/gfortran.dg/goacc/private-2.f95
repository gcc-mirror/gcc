! { dg-do compile }

! test for implicit private clauses in do loops

program test
  implicit none
  integer :: i, j, k, a(10)

  !$acc parallel
  !$acc loop
  do i = 1, 100
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop
  do i = 1, 100
     do j = 1, 100
     end do
  end do
  !$acc end parallel

  !$acc data copy(a)

  if(mod(1,10) .eq. 0) write(*,'(i5)') i

  do i = 1, 100
    !$acc parallel
    !$acc loop
     do j = 1, 100
        do k = 1, 100
        end do
     end do
    !$acc end parallel
  end do

  !$acc end data

end program test
