! { dg-do compile { target aarch64*-*-* } }
! { dg-options "-O2 -funsafe-math-optimizations -fdump-rtl-combine" }

subroutine f(vara,varb,varc,res)
      REAL, INTENT(IN) :: vara,varb,varc
      REAL, INTENT(out) :: res

      res = vara
      if (res .lt. varb)  res = varb
      if (res .gt. varc)  res = varc
end subroutine

! { dg-final { scan-rtl-dump-not "smin" "combine" } }
