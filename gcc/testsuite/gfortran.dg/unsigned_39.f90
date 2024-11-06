! { dg-do run }
! { dg-options "-funsigned" }
program memain
  use iso_fortran_env, only : uint8, uint32
  implicit none
  call test1
  call test2
contains
  subroutine test1
    unsigned(uint32) :: u1, u2
    unsigned(uint8), dimension(3,3) :: v1, v2
    u1 = umaskr(3)
    if (u1 /= 7u) error stop 1
    u2 = umaskl(2)
    if (u2 /= 3221225472u) error stop 2
    v1 = umaskr(5,uint8)
    if (any(v1 /= 31u)) error stop 3
    v2 = umaskl(5,uint8)
    if (any(v2 /= 248u_uint8)) error stop 4
  end subroutine test1
  subroutine test2
    unsigned(uint32), parameter :: u1 = umaskr(3), u2=umaskl(2)
    unsigned(uint8), dimension(3,3) :: v1 = umaskr(5,uint8), v2 = umaskl(5,uint8)
    if (u1 /= 7u) error stop 11
    if (u2 /= 3221225472u) error stop 12
    if (any(v1 /= 31u)) error stop 13
    if (any(v2 /= 248u_uint8)) error stop 14
  end subroutine test2
end program memain
