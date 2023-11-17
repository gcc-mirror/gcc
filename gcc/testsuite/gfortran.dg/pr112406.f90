! { dg-do compile { target { aarch64-*-* || riscv*-*-* } } }
! { dg-options "-Ofast -w -fprofile-generate" }
! { dg-additional-options "-march=rv64gcv -mabi=lp64d" { target riscv*-*-* } }
! { dg-additional-options "-march=armv8-a+sve" { target aarch64-*-* } }

module brute_force
  integer, parameter :: r=9
   integer sudoku1(1, r)
  contains
subroutine brute
integer l(r), u(r)
   where(sudoku1(1, :) /= 1)
        l = 1
      u = 1
   end where
do i1 = 1, u(1)
   do
      end do
   end do
end
end
