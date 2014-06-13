! { dg-do compile }

subroutine f6
!$omp declare reduction (foo:real:omp_out (omp_in)) ! { dg-error "Unclassifiable OpenMP directive" }
!$omp declare reduction (bar:real:omp_out = omp_in * omp_out) & ! { dg-error "Unclassifiable OpenMP directive" }
!$omp & initializer (omp_priv (omp_orig))
end subroutine f6
subroutine f7
  integer :: a
!$omp declare reduction (foo:integer:a (omp_out, omp_in)) ! { dg-error "Unclassifiable OpenMP directive" }
!$omp declare reduction (bar:real:omp_out = omp_out.or.omp_in) ! { dg-error "Operands of logical operator" }
!$omp declare reduction (baz:real:omp_out = omp_out + omp_in)
!$omp & initializer (a (omp_priv, omp_orig)) ! { dg-error "Unclassifiable OpenMP directive" }
end subroutine f7
subroutine f8
  interface
    subroutine f8a (x)
      integer :: x
    end subroutine f8a
  end interface
!$omp declare reduction (baz:integer:omp_out = omp_out + omp_in) &
!$omp & initializer (f8a (omp_orig)) ! { dg-error "One of actual subroutine arguments in INITIALIZER clause" }
!$omp declare reduction (foo:integer:f8a) ! { dg-error "is not a variable" }
!$omp declare reduction (bar:integer:omp_out = omp_out - omp_in) &
!$omp & initializer (f8a) ! { dg-error "is not a variable" }
end subroutine f8
subroutine f9
  type dt	! { dg-error "which is not consistent with the CALL" }
    integer :: x = 0
    integer :: y = 0
  end type dt
!$omp declare reduction (foo:integer:dt (omp_out, omp_in)) ! { dg-error "which is not consistent with the CALL" }
!$omp declare reduction (bar:integer:omp_out = omp_out + omp_in) &
!$omp & initializer (dt (omp_priv, omp_orig)) ! { dg-error "which is not consistent with the CALL" }
end subroutine f9
subroutine f10
  integer :: a, b
!$omp declare reduction(foo:character(len=64) &
!$omp & :omp_out(a:b) = omp_in(a:b)) ! { dg-error "Variable other than OMP_OUT or OMP_IN used in combiner" }
!$omp declare reduction(bar:character(len=16) &
!$omp & :omp_out = trim(omp_out) // omp_in) &
!$omp & initializer (omp_priv(a:b) = ' ') ! { dg-error "Variable other than OMP_PRIV or OMP_ORIG used in INITIALIZER clause" }
end subroutine f10
