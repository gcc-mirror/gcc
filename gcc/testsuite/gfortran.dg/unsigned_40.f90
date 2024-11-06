! { dg-do run }
! { dg-options "-funsigned" }
program memain
  use iso_fortran_env, only : uint8
  call test1
  call test2
contains
  subroutine test1
    unsigned(uint8) :: nface, nmax
    nface = 12u_1
    nmax = - mod(-nface+1u,nface)
    if (nmax /= 251u_1) error stop 1
  end subroutine test1
  subroutine test2
    unsigned(uint8), parameter :: nface = 12u_1
    unsigned(uint8), parameter :: nmax = - mod(-nface+1u,nface)
    if (nmax /= 251u_1) error stop 11
  end subroutine test2
end program memain
