! { dg-do compile }

! Ensure that type(C_ptr) check is done at resolve rather than parse time


module m
  use iso_c_binding
  implicit none (type, external)
contains
  subroutine foo(x,y)
    !$omp declare variant(bar) match ( construct = { dispatch } ) adjust_args(nothing : x ) adjust_args(need_device_ptr : y )
    type(C_ptr), value :: x, y
  end
  subroutine bar(a,b)
    type(C_ptr), value :: a, b
  end
end 
