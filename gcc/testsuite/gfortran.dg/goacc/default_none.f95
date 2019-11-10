! Ensure that the internal array variables, offset, lbound, etc., don't
! trigger errors with default(none).

! { dg-do compile }

program main
  implicit none
  integer i
  integer,parameter :: n = 100
  integer,allocatable :: a1(:), a2(:,:)

  allocate (a1 (n))
  allocate (a2 (-n:n,-n:n))
  a1 (:) = -1

  !$acc parallel loop default(none) copy (a1(1:n))
  do i = 1,n
     a1(i) = a1(i) + 1 * i
  end do
  !$acc end parallel loop

  !$acc serial loop default(none) copy (a1(1:n))
  do i = 1,n
     a1(i) = a1(i) + 3 * i
  end do
  !$acc end serial loop

  call foo (a1)
  call bar (a1, n)
  call foobar (a2,n)

contains

  subroutine foo (da1)
    integer :: da1(n)

    !$acc parallel loop default(none) copy (da1(1:n))
    do i = 1,n
       da1(i) = da1(i) + 1 * i * 2
    end do
    !$acc end parallel loop

    !$acc serial loop default(none) copy (da1(1:n))
    do i = 1,n
       da1(i) = da1(i) + 3 * i * 2
    end do
    !$acc end serial loop

  end subroutine foo
end program main

subroutine bar (da2,n)
  integer :: n, da2(n)
  integer i

  !$acc parallel loop default(none) copy (da2(1:n)) firstprivate(n)
  do i = 1,n
     da2(i) = da2(i) + 1 * i * 3
  end do
  !$acc end parallel loop

  !$acc serial loop default(none) copy (da2(1:n)) firstprivate(n)
  do i = 1,n
     da2(i) = da2(i) + 1 * i * 3
  end do
  !$acc end serial loop

end subroutine bar

subroutine foobar (da3,n)
  integer :: n, da3(-n:n,-n:n)
  integer i

  !$acc parallel loop default(none) copy (da3(-n:n,-n:n)) firstprivate(n)
  do i = 1,n
     da3(i, 0) = da3(i, 0) + 1 * i * 3
  end do
  !$acc end parallel loop

  !$acc serial loop default(none) copy (da3(-n:n,-n:n)) firstprivate(n)
  do i = 1,n
     da3(i, 0) = da3(i, 0) + 1 * i * 3
  end do
  !$acc end serial loop

end subroutine foobar
