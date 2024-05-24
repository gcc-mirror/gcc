! { dg-do compile }

! Check for proper error recovery in resolve_omp_dispatch

module m
  use iso_c_binding
  implicit none (type, external)
contains
  subroutine foo(x,y)
    !$omp declare variant(bar) match ( construct = { dispatch } )
    type(C_ptr), value :: x, y
  end
  subroutine bar(a,b)
    type(C_ptr), value :: a, b
  end
end

use m
  integer :: y, z
  !$omp dispatch device(5)
    call foo(c_loc(y),c_loc(z)) !{ dg-error "Argument X at .1. to C_LOC shall have either the POINTER or the TARGET attribute" }
end 
