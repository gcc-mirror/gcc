! { dg-do compile }
! { dg-options "-fno-move-loop-invariants -Oz" }
module module_foo
  use iso_c_binding
  contains
  subroutine foo(a) bind(c)
    type(c_ptr)  a(..)
    select rank(a)
    end select
    call bar
  end
end
