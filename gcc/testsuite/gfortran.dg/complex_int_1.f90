! { dg-do compile }
! Complex constants with integer components should take ther kind from
! the real typed component, or default complex type if both components have
! integer type.
program prog
  call test1 ((1_8, 1.0_4))
  call test2 ((1_8, 2_8))
contains
subroutine test1(x)
  complex(4) :: x
end subroutine
subroutine test2(x)
  complex :: x
end subroutine
end program
