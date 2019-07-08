! { dg-do assemble { target aarch64_asm_sve_ok } }
! { dg-options "-O3 -march=armv8.2-a+sve --save-temps" }

subroutine foo(x)
  real :: x(100)
  x = x + 10
end subroutine foo

! { dg-final { scan-assembler {\twhilelo\tp[0-9]+\.s, wzr, (w[0-9]+).*\twhilelo\tp[0-9]+\.s, w[0-9]+, \1} } }
