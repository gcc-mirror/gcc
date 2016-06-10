! OpenACC cache directive: valid usage.
! For execution testing, this file is "#include"d from
! libgomp/testsuite/libgomp.oacc-fortran/cache-1.f95.
! { dg-additional-options "-std=f2008" }

program test
  implicit none
  integer :: i, d(10), e(5,13)

  do concurrent (i=1:5)
    !$acc cache (d(1:3))
    !$acc cache (d(i:i+2))
    !$acc cache (e(1:3,2:4))
    !$acc cache (e(i:i+2,i+1:i+3))
  enddo
end
