! { dg-do compile }
! PR43747 ICE in find_array_section, at fortran/expr.c:1551
! Test case by Dominique d'Humieres
INTEGER, PARAMETER ::N=65536
INTEGER, PARAMETER ::I(N)=(/(MOD(K,2),K=1,N)/)!{ dg-error "number of elements" }
INTEGER, PARAMETER ::M(N)=I(N:1:-1) ! { dg-error "Syntax error in argument" }
print *, I(1), M(1), I(N), M(N)
END

