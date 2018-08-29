! { dg-do run }
! { dg-additional-sources c_size_t_driver.c }
module c_size_t_test
  use, intrinsic :: iso_c_binding

contains
  subroutine sub0(my_c_size) bind(c)
    integer(c_int), value :: my_c_size ! value of C's sizeof(size_t)

    ! if the value of c_size_t isn't equal to the value of C's sizeof(size_t) 
    ! we call abort.
    if(c_size_t .ne. my_c_size) then
       STOP 1
    end if
  end subroutine sub0
end module c_size_t_test
