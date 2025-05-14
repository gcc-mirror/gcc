! { dg-additional-options "-Wunused-variable" }
implicit none
integer :: i, j, k, ll
integer :: jj, kk, lll
do , concurrent (i = 1:5) shared(j,jj) local(k,kk) local_init(ll,lll)
    ! { dg-warning "Variable 'kk' in locality-spec at \\(1\\) is not used \\\[-Wunused-variable\\\]" "" { target *-*-* } .-1 }
    ! { dg-warning "Variable 'll' in locality-spec at \\(1\\) is not used \\\[-Wunused-variable\\\]" "" { target *-*-* } .-2 }
    ! { dg-warning "Variable 'jj' in locality-spec at \\(1\\) is not used \\\[-Wunused-variable\\\]" "" { target *-*-* } .-3 }
  j = 5
  k = 7
  lll = 8
end do
end
