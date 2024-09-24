! { dg-do run }
! { dg-options "-funsigned" }
! Test dot_product both for runtime and compile
program memain
  call test1
  call test2
contains
  subroutine test1
    integer, parameter :: n = 10
    real(8), dimension(n) :: a, b
    unsigned, dimension(n) :: u, v
    integer(8), dimension(n) :: i, j
    unsigned :: res_u
    integer(8) :: res_i
    integer :: k

    do k=1,10
       call random_number(a)
       call random_number(b)
       u = uint(a*2**32)
       v = uint(b*2**32)
       i = int(u,8)
       j = int(v,8)
       res_u = dot_product(u,v)
       res_i = dot_product(i,j)
       if (res_u /= uint(res_i)) error stop 1
    end do
  end subroutine test1

  subroutine test2
    integer, parameter :: n = 5
    unsigned, parameter, dimension(n) :: &
      u = [1149221887u,  214388752u,  724301838u, 1618160523u, 1783282425u], &
      v = [1428464973u, 1887264271u, 1830319906u,  983537781u,   13514400u]
    integer(8), parameter, dimension(n) :: i = int(u,8), j=int(v,8)
    unsigned, parameter :: res_1 = dot_product(u,v)
    integer(8), parameter :: res_2 = dot_product(i,j)
    if (res_1 /= uint(res_2)) error stop 2
  end subroutine test2
end program
