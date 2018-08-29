! See also loop-1-2.f95.
! { dg-additional-options "-std=legacy" }

module test
  implicit none
contains

subroutine test1
  integer :: i, j, k, b(10)
  integer, dimension (30) :: a
  double precision :: d
  real :: r
  i = 0
  !$acc loop
  do 100 ! { dg-error "cannot be a DO WHILE or DO without loop control" }
    if (i .gt. 0) exit ! { dg-error "EXIT statement" }
  100 i = i + 1
  i = 0
  !$acc loop
  do ! { dg-error "cannot be a DO WHILE or DO without loop control" }
      if (i .gt. 0) exit ! { dg-error "EXIT statement" }
       i = i + 1
  end do
  i = 0
  !$acc loop
  do 200 while (i .lt. 4) ! { dg-error "cannot be a DO WHILE or DO without loop control" }
  200 i = i + 1
  !$acc loop
  do while (i .lt. 8) ! { dg-error "cannot be a DO WHILE or DO without loop control" }
       i = i + 1
  end do
  !$acc loop
  do 300 d = 1, 30, 6
      i = d
  300 a(i) = 1
  ! { dg-error "ACC LOOP iteration variable must be of type integer" "" { target *-*-* } 33 }
  !$acc loop
  do d = 1, 30, 5
       i = d
      a(i) = 2
  end do
  ! { dg-error "ACC LOOP iteration variable must be of type integer" "" { target *-*-* } 38 }
  !$acc loop
  do i = 1, 30
      if (i .eq. 16) exit ! { dg-error "EXIT statement" }
  end do
  !$acc loop
  outer: do i = 1, 30
      do j = 5, 10
          if (i .eq. 6 .and. j .eq. 7) exit outer ! { dg-error "EXIT statement" }
      end do
  end do outer
  last: do i = 1, 30
   end do last

  ! different types of loop are allowed
  !$acc loop
  do i = 1,10
  end do
  !$acc loop
  do 400, i = 1,10
400   a(i) = i

  ! after loop directive must be loop
  !$acc loop
  a(1) = 1 ! { dg-error "Expected DO loop" }
  do i = 1,10
  enddo

  ! combined directives may be used with/without end
  !$acc parallel loop
  do i = 1,10
  enddo
  !$acc parallel loop
  do i = 1,10
  enddo
  !$acc end parallel loop
  !$acc kernels loop
  do i = 1,10
  enddo
  !$acc kernels loop
  do i = 1,10
  enddo
  !$acc end kernels loop

  !$acc kernels loop reduction(max:i)
  do i = 1,10
  enddo
  !$acc kernels
  !$acc loop reduction(max:i)
  do i = 1,10
  enddo
  !$acc end kernels

  !$acc parallel loop collapse(0) ! { dg-error "constant positive integer" }
  do i = 1,10
  enddo

  !$acc parallel loop collapse(-1) ! { dg-error "constant positive integer" }
  do i = 1,10
  enddo

  !$acc parallel loop collapse(i) ! { dg-error "Constant expression required" }
  do i = 1,10
  enddo

  !$acc parallel loop collapse(4) ! { dg-error "not enough DO loops for collapsed" }
    do i = 1, 3
        do j = 4, 6
          do k = 5, 7
              a(i+j-k) = i + j + k
          end do
        end do
    end do
    !$acc parallel loop collapse(2)
    do i = 1, 5, 2
        do j = i + 1, 7, i  ! { dg-error "collapsed loops don.t form rectangular iteration space" }
        end do
    end do
    !$acc parallel loop collapse(2)
    do i = 1, 3
        do j = 4, 6
        end do
    end do
    !$acc parallel loop collapse(2)
    do i = 1, 3
        do j = 4, 6
        end do
        k = 4
    end do
    !$acc parallel loop collapse(3-1)
    do i = 1, 3
        do j = 4, 6
        end do
        k = 4
    end do
    !$acc parallel loop collapse(1+1)
    do i = 1, 3
        do j = 4, 6
        end do
        k = 4
    end do
    !$acc parallel loop collapse(2)
    do i = 1, 3
        do      ! { dg-error "cannot be a DO WHILE or DO without loop control" }
        end do
    end do
    !$acc parallel loop collapse(2)
    do i = 1, 3
        do r = 4, 6
        end do
        ! { dg-error "ACC LOOP iteration variable must be of type integer" "" { target *-*-* } 150 }
    end do

    ! Both seq and independent are not allowed
  !$acc loop independent seq ! { dg-error "SEQ conflicts with INDEPENDENT" }
  do i = 1,10
  enddo


  !$acc cache (a(1:10)) ! { dg-error "ACC CACHE directive must be inside of loop" }

  do i = 1,10
    !$acc cache(a(i:i+1))
  enddo

  do i = 1,10
    !$acc cache(a(i:i+1))
    a(i) = i
    !$acc cache(a(i+2:i+2+1))
  enddo

end subroutine test1
end module test
