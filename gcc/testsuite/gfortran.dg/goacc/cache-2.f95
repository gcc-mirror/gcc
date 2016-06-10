! OpenACC cache directive: invalid usage.
! { dg-additional-options "-std=f2008" }

program test
  implicit none
  integer :: i, d(10), e(5,13)

  do concurrent (i=1:5)
    !$acc cache (d) ! { dg-error "" "TODO" { xfail *-*-* } }
    !$acc cache (e) ! { dg-error "" "TODO" { xfail *-*-* } }
  enddo
end
