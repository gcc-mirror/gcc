! { dg-do run }
! { dg-options "-funsigned" }
program memain
  implicit none
  call test1
  call test2
contains
  subroutine test1
    unsigned, dimension(3) :: v
    unsigned :: t1, t2
    unsigned(2), dimension(3,3) :: w
    integer, dimension(3,3) :: j
    integer :: di
    v = [1u, 2u, 4294967286u]
    t1 = maxval(v,dim=1) 
    if (t1 /= 4294967286u) error stop 1
    t2 = minval(v,dim=1)
    if (t2 /= 1u) error stop 2
    call check_empty(0)
    j = reshape([1,2,3,65534,5,1,65000,2,1],[3,3])
    w = uint(j,2)
    if (any(maxval(j,dim=1) /= int(maxval(w,dim=1)))) error stop 5
    di = 2
    if (any(maxval(j,dim=di) /= int(maxval(w,dim=di)))) error stop 6
  end subroutine test1
  subroutine check_empty(n)
    integer, intent(in) :: n
    unsigned, dimension(n) :: empty
    if (minval(empty,dim=1) /= 4294967295u) error stop 3
    if (maxval(empty,dim=1) /= 0u) error stop 4
  end subroutine check_empty
  subroutine test2
    integer :: i
    unsigned, dimension(3), parameter :: v = [1u, 2u, 4294967286u]
    unsigned, parameter :: t1 = maxval(v,dim=1)
    unsigned, parameter :: t2 = minval(v,dim=1)
    unsigned, parameter, dimension(2:1) :: empty = [(0u,i=2,1)]
    unsigned, parameter :: t3 = minval(empty,1)
    unsigned, parameter :: t4 = maxval(empty,1)
    unsigned(2), parameter, dimension(2:1,2:1) :: e2 = reshape(empty,[0,0])
    integer, parameter, dimension(3,3) :: j = reshape([1,2,3,65534,5,1,65000,2,1],[3,3])
    integer, parameter, dimension(3) :: maxvj = maxval(j,1), minvj=minval(j,2)
    unsigned, parameter, dimension(3,3) :: w = uint(j,2)
    unsigned(2), parameter, dimension(3) :: maxvw = maxval(w,1), minvw = minval(w,2)

    if (t1 /= 4294967286u) error stop 11
    if (t2 /= 1u) error stop 12
    if (t3 /= 4294967295u) error stop 13
    if (t4 /= 0u) error stop 14
    if (any(maxvj /= int(maxvw))) error stop 15
    if (any(minvj /= int(minvw))) error stop 16
  end subroutine test2
end program memain
