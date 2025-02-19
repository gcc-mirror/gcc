module m
  implicit none
contains
  subroutine f(x,y,z)
    integer:: x, y, z
    value :: y
  end subroutine
  subroutine f0(x,y,z)
    !$omp declare variant(f) adjust_args ( need_device_addr : : omp_num_args-1) &
    !$omp&                   adjust_args ( need_device_ptr : z) &
    !$omp&                   match ( construct = { dispatch } )
    integer:: x, y, z
    value :: y

! { dg-error "19: Argument 'y' at .1. to list item in 'need_device_addr' at .2. must not have the VALUE attribute" "" { target *-*-* } 8 }
! { dg-error "62: Argument 'y' at .1. to list item in 'need_device_addr' at .2. must not have the VALUE attribute" "" { target *-*-* } 9 }
! { dg-message "sorry, unimplemented: 'need_device_addr' not yet supported" "" { target *-*-* } 9 }

! { dg-error "Argument 'z' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" "" { target *-*-* } 8 }
! { dg-error "Argument 'z' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" "" { target *-*-* } 10 }
! { dg-note "Consider using 'need_device_addr' instead" "" { target *-*-* } 10 }
  end subroutine
end module m

module m2
  use iso_c_binding, only: c_ptr
  implicit none
 interface
  subroutine f(x,y,z)
    import
    type(c_ptr) :: x, y, z
  end subroutine
  subroutine f0(x,y,z)
    import
    type(c_ptr) :: x, y, z
    !$omp declare variant(f) adjust_args ( need_device_ptr : : ) &
    !$omp&                   adjust_args ( nothing : 2, 4) &
    !$omp&                   match ( construct = { dispatch } )

! { dg-error "54: 'y' at .1. is specified more than once" "" { target *-*-* } 37 }
! { dg-warning "57: Argument index at .1. exceeds number of arguments 3 \\\[-Wopenmp\\\]" "" { target *-*-* } 37 }
  end subroutine
 end interface
end module m2

module m3
  use iso_c_binding, only: c_ptr
  implicit none
 interface
  subroutine f(x,y,z)
    import
    type(c_ptr) :: x, y, z
  end subroutine
  subroutine f0(x,y,z)
    import
    type(c_ptr) :: x, y, z
    !$omp declare variant(f) adjust_args ( need_device_addr : omp_num_args -4 :, 3 : 2) &
    !$omp&                   match ( construct = { dispatch } )
! { dg-warning "63: Expected positive argument index at .1. \\\[-Wopenmp\\\]" "" { target *-*-* } .-2 }
! { dg-warning "82: Upper argument index smaller than lower one at .1. \\\[-Wopenmp\\\]" "" { target *-*-* } .-3 }
  end subroutine
 end interface
end module m3

module m4
  use iso_c_binding, only: c_ptr
  implicit none
 interface
  subroutine f(x,y,z)
    import
    type(c_ptr) :: x, y, z
  end subroutine
  subroutine f0(x,y,z)
    import
    type(c_ptr) :: x, y, z
    !$omp declare variant(f) adjust_args ( need_device_addr : x, y, omp_num_args -2 : omp_num_args -1) &
    !$omp&                   adjust_args ( need_device_addr : z) &
    !$omp&                   adjust_args ( need_device_addr : omp_num_args : 3) &
    !$omp&                   match ( construct = { dispatch } )
! { dg-error "69: 'x' at .1. is specified more than once" "" { target *-*-* } .-4 }
! { dg-error "69: 'y' at .1. is specified more than once" "" { target *-*-* } .-5 }
! { dg-error "63: 'z' at .1. is specified more than once" "" { target *-*-* } .-4 }
  end subroutine
 end interface
end module m4
