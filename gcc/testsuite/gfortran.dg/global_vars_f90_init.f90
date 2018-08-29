! { dg-do run }
! { dg-additional-sources global_vars_f90_init_driver.c }
module global_vars_f90_init
  use, intrinsic :: iso_c_binding, only: c_int
  implicit none

  integer(c_int), bind(c, name='i') :: I = 2

contains
  subroutine test_globals() bind(c)
    ! the value of I is initialized above
    if(I .ne. 2) then
       STOP 1
    endif
  end subroutine test_globals
end module global_vars_f90_init
