! { dg-do compile }
!
! PR fortran/50050
! Out of bound whilst releasing initialization of allocate object
!
! Contributed by someone <sigurdkn@gmail.com>

program bug
  implicit none
  type foo
    integer, pointer :: a => null()
  end type
  type(foo), dimension(:,:), allocatable :: data
  allocate(data(1:1,1)) ! This used to lead to an ICE
end program
