! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }

subroutine test1(n)
  implicit none
    integer(8) :: n
    real(4), allocatable :: z(:,:,:)

    allocate(z(n, 100, 200))
    z = 0
end subroutine

subroutine test2(n)
  implicit none
    integer(8) :: n
    integer, allocatable :: z(:,:,:)

    allocate(z(n, 100, 200))
    z = 0
end subroutine

subroutine test3(n)
  implicit none
    integer(8) :: n
    logical, allocatable :: z(:,:,:)

    allocate(z(n, 100, 200))
    z = .false. 
end subroutine

subroutine test4(n, z)
   implicit none
   integer :: n
   real, pointer :: z(:,:,:)     ! need not be contiguous!
   z = 0
end subroutine

subroutine test5(n, z)
   implicit none
   integer :: n
   real, contiguous, pointer :: z(:,:,:)
   z = 0
end subroutine

subroutine test6 (n, z)
   implicit none
   integer :: n
   real, contiguous, pointer :: z(:,:,:)
   z(:,::1,:) = 0
end subroutine

! { dg-final { scan-tree-dump-times "__builtin_memset" 5 "original" } }
