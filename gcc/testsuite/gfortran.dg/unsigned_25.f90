! { dg-do run }
! { dg-options "-funsigned" }
! Test matrix multiplication
program memain
  implicit none
  call test1
  call test2
contains
  subroutine test1
    integer, parameter :: n = 10, m = 28
    unsigned, dimension(n,n) :: u, v, w
    integer(kind=8), dimension(n,n) :: i, j, k
    real(8), dimension(n,n) :: a, b

    call random_number(a)
    call random_number(b)
    u = uint(a*2.0**m)
    v = uint(b*2.0**m)
    i = int(a*2.0**m,8)
    j = int(b*2.0**m,8)
    w = matmul(u,v)
    k = mod(matmul(i,j),2_8**32)
    if (any(uint(k) /= w)) error stop 1
  end subroutine test1
  subroutine test2
    unsigned, parameter :: u(3,3) = reshape ([1u, uint(-2), 3u, uint(-4), &
         5u, uint(-6), 7u, uint(-8), 9u],[3,3])
    unsigned, parameter :: v(3,3) = 1u - u
    unsigned, parameter :: w(3,3) = matmul(u,v)
    integer(kind=8), dimension(3,3), parameter :: &
         i = int(u,8), j = int(v,8)
    integer(kind=8), dimension(3,3) :: k = matmul(i,j)
    if (any(uint(k) /= w)) error stop 2
  end subroutine test2
end program memain
