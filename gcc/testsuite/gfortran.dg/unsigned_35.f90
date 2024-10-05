! { dg-do run }
! { dg-options "-funsigned" }
program memain
  implicit none
  call test1
contains
  subroutine test1
    unsigned, dimension(3) :: v
    integer :: t1, t2
    unsigned(2), dimension(3,3) :: w
    integer, dimension(3,3) :: j
    integer :: di
    v = [1u, 2u, 4294967286u]
    t1 = maxloc(v,dim=1) 
    if (t1 /= 3) error stop 1
    t2 = minloc(v,dim=1)
    if (t2 /= 1) error stop 2
    call check_empty(0)
    j = reshape([1,2,3,65534,5,1,65000,2,1],[3,3])
    w = uint(j,2)
    if (any(maxloc(j,dim=1) /= int(maxloc(w,dim=1)))) error stop 5
    di = 2
    if (any(maxloc(j,dim=di) /= int(maxloc(w,dim=di)))) error stop 6
  end subroutine test1
  subroutine check_empty(n)
    integer, intent(in) :: n
    unsigned, dimension(n) :: empty
    if (minloc(empty, dim=1) /= 0) error stop 3
    if (maxloc(empty, dim=1) /= 0) error stop 4
  end subroutine check_empty
  subroutine test2
    integer :: i
    unsigned, dimension(3), parameter :: v = [1u, 2u, 4294967286u]
    integer, parameter :: t1 = maxloc(v,dim=1)
    integer, parameter :: t2 = minloc(v,dim=1)
    unsigned, parameter, dimension(2:1) :: empty = [(0u,i=2,1)]
    integer, parameter :: t3 = minloc(empty,1)
    integer, parameter :: t4 = maxloc(empty,1)
    unsigned(2), parameter, dimension(2:1,2:1) :: e2 = reshape(empty,[0,0])
    integer, parameter, dimension(3,3) :: j = reshape([1,2,3,65534,5,1,65000,2,1],[3,3])
    integer, parameter, dimension(3) :: maxvj = maxloc(j,1), minvj=minloc(j,2)
    unsigned, parameter, dimension(3,3) :: w = uint(j,2)
    integer(2), parameter, dimension(3) :: maxvw = maxloc(w,1), minvw = minloc(w,2)

    if (t1 /= 3) error stop 11
    if (t2 /= 1) error stop 12
    if (t3 /= 0) error stop 13
    if (t4 /= 0) error stop 14
    if (any(maxvj /= maxvw)) error stop 15
    if (any(minvj /= minvw)) error stop 16
  end subroutine test2
end program memain
