! { dg-do compile }
module bind_c_procs
  use, intrinsic :: iso_c_binding, only: c_int

  interface
     ! warning for my_param possibly not being C interoperable
     subroutine my_c_sub(my_param) bind(c) ! { dg-warning "may not be C interoperable" }
       integer, value :: my_param
     end subroutine my_c_sub

     ! warning for my_c_func possibly not being a C interoperable kind
     ! warning for my_param possibly not being C interoperable
     ! error message truncated to provide an expression that both warnings
     ! should match.
     function my_c_func(my_param) bind(c) ! { dg-warning "may not be" }
       integer, value :: my_param
       integer :: my_c_func
     end function my_c_func
  end interface

contains
  ! warning for my_param possibly not being C interoperable
  subroutine my_f03_sub(my_param) bind(c) ! { dg-warning "may not be" }
    integer, value :: my_param
  end subroutine my_f03_sub

  ! warning for my_f03_func possibly not being a C interoperable kind
  ! warning for my_param possibly not being C interoperable
  ! error message truncated to provide an expression that both warnings
  ! should match.
  function my_f03_func(my_param) bind(c) ! { dg-warning "may not be" }
    integer, value :: my_param
    integer :: my_f03_func
    my_f03_func = 1
  end function my_f03_func

end module bind_c_procs

! { dg-final { cleanup-modules "bind_c_procs" } }
