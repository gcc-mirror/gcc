! { dg-do run }
! { dg-additional-options "-fsanitize=undefined -fcheck=all" }

! PR fortran/92621

subroutine hello(val) bind(c)
  use, intrinsic :: iso_c_binding, only: c_int

  implicit none
  
  integer(kind=c_int), allocatable, intent(out) :: val(:)

  allocate(val(1))
  val = 2
  return
end subroutine hello

program alloc_p

  use, intrinsic :: iso_c_binding, only: c_int

  implicit none

  interface
    subroutine hello(val) bind(c)
      import :: c_int
      implicit none
      integer(kind=c_int), allocatable, intent(out) :: val(:)
    end subroutine hello
  end interface

  integer(kind=c_int), allocatable :: a(:)

  allocate(a(1))
  a = 1
  call hello(a)
  stop

end program alloc_p
