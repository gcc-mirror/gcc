! { dg-do compile }

! Check that duplicate adjust_args list items are reported

module m
  use iso_c_binding
  implicit none (type, external)
contains
  subroutine foo(x,y)
    type(C_ptr), value :: x, y
    !$omp declare variant(bar) match ( construct = { dispatch } ) adjust_args(nothing : x ,y ) adjust_args(need_device_ptr : y )   !{ dg-error "'y' at .1. is specified more than once" }
  end
  subroutine bar(a,b)
    type(C_ptr), value :: a, b  ! OK
  end
end 
