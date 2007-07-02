! { dg-do run }
! { dg-additional-sources bind_c_dts_driver.c }
module bind_c_dts
  use, intrinsic :: iso_c_binding
  implicit none

  type, bind(c) :: MYFTYPE_1
     integer(c_int) :: i, j
     real(c_float) :: s
  end type MYFTYPE_1

  TYPE, BIND(C) :: particle
     REAL(C_DOUBLE) :: x,vx
     REAL(C_DOUBLE) :: y,vy
     REAL(C_DOUBLE) :: z,vz
     REAL(C_DOUBLE) :: m
  END TYPE particle

  type(myftype_1), bind(c, name="myDerived") :: myDerived

contains
  subroutine types_test(my_particles, num_particles) bind(c)
    integer(c_int), value :: num_particles
    type(particle), dimension(num_particles) :: my_particles
    integer :: i

    ! going to set the particle in the middle of the list
    i = num_particles / 2;
    my_particles(i)%x = my_particles(i)%x + .2d0
    my_particles(i)%vx = my_particles(i)%vx + .2d0
    my_particles(i)%y = my_particles(i)%y + .2d0
    my_particles(i)%vy = my_particles(i)%vy + .2d0
    my_particles(i)%z = my_particles(i)%z + .2d0
    my_particles(i)%vz = my_particles(i)%vz + .2d0
    my_particles(i)%m = my_particles(i)%m + .2d0

    myDerived%i = myDerived%i + 1
    myDerived%j = myDerived%j + 1
    myDerived%s = myDerived%s + 1.0;
  end subroutine types_test
end module bind_c_dts
