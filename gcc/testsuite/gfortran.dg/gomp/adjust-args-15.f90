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
    !$omp declare variant(f) adjust_args ( need_device_addr : -1 : omp_num_args + 10 ) & ! { dg-error "64: For range-based 'adjust_args', a constant positive scalar integer expression is required" }
    !$omp&                   adjust_args ( nothing : 1+1) &  ! { dg-error "expected ':'" }
    !$omp&                   match ( construct = { dispatch } )
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
    !$omp declare variant(f) adjust_args ( need_device_addr : 3.3 ) &  ! { dg-error "Expected dummy parameter name or a positive integer" }
    !$omp&                   adjust_args ( nothing : 1 : y ) &  ! { dg-error "For range-based 'adjust_args', a constant positive scalar integer expression is required" }
    !$omp&                   match ( construct = { dispatch } )
  end subroutine
 end interface
end module m4
