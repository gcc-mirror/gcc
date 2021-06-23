! { dg-do compile }
! { dg-options "-std=gnu" }
! PR fortran/100283

subroutine s ()
  integer(kind=8) :: i,j,k
  i = min0 (j,k)
  i = max0 (-127_8, min0 (j,127_8))
end subroutine s
