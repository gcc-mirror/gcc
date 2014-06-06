! { dg-do compile }

subroutine f3
!$omp declare reduction ! { dg-error "Unclassifiable OpenMP directive" }
!$omp declare reduction foo ! { dg-error "Unclassifiable OpenMP directive" }
!$omp declare reduction (foo) ! { dg-error "Unclassifiable OpenMP directive" }
!$omp declare reduction (foo:integer) ! { dg-error "Unclassifiable OpenMP directive" }
!$omp declare reduction (foo:integer:omp_out=omp_out+omp_in) &
!$omp & initializer(omp_priv=0) initializer(omp_priv=0) ! { dg-error "Unclassifiable statement" }
end subroutine f3
subroutine f4
  implicit integer (o)
  implicit real (b)
!$omp declare reduction (foo:integer:omp_priv(omp_out,omp_in)) ! { dg-error "Implicitly declared subroutine omp_priv" }
!$omp declare reduction (foo:real:bar(omp_out,omp_in)) ! { dg-error "Implicitly declared subroutine bar used" }
!$omp declare reduction (bar:integer:omp_out=omp_out+omp_in) &
!$omp & initializer(omp_out (omp_priv)) ! { dg-error "Implicitly declared subroutine omp_out used" }
!$omp declare reduction (bar:real:omp_out=omp_out+omp_in) &
!$omp & initializer(bar (omp_priv, omp_orig)) ! { dg-error "Implicitly declared subroutine bar used" }
!$omp declare reduction (id1:integer:omp_out=omp_orig(omp_out,omp_in)) ! { dg-error "Implicitly declared function omp_orig used" }
!$omp declare reduction (id1:real:omp_out=foo(omp_out,omp_in)) ! { dg-error "Implicitly declared function foo used" }
!$omp declare reduction (id2:integer:omp_out=omp_out+omp_in) &
!$omp & initializer(omp_priv = omp_in (omp_orig)) ! { dg-error "Implicitly declared function omp_in used" }
!$omp declare reduction (id2:real:omp_out=omp_out+omp_in) &
!$omp & initializer(omp_priv = baz (omp_orig)) ! { dg-error "Implicitly declared function baz used" }
end subroutine f4
subroutine f5
  interface
    subroutine f5a (x, *, y)
      double precision :: x, y
    end subroutine f5a
  end interface
!$omp declare reduction (foo:double precision: & ! { dg-error "Subroutine call with alternate returns in combiner" }
!$omp & f5a (omp_out, *10, omp_in))
!$omp declare reduction (bar:double precision: &
!$omp omp_out = omp_in + omp_out) &
!$omp & initializer (f5a (omp_priv, *20, omp_orig)) ! { dg-error "Subroutine call with alternate returns in INITIALIZER clause" }
10 continue
20 continue
! { dg-error "Label\[^\n\r]* is never defined" "" { target *-*-* } 0 }
! { dg-prune-output "<During initialization>" }
end subroutine f5
subroutine f6
  integer :: a
!$omp declare reduction(foo:character(len=a*2) & ! { dg-error "cannot appear in the expression|not constant" }
!$omp & :omp_out=trim(omp_out)//omp_in) &
!$omp & initializer(omp_priv=' ')
end subroutine f6
subroutine f7
  type dt1
    integer :: a = 1
    integer :: b
  end type
  type dt2
    integer :: a = 2
    integer :: b = 3
  end type
  type dt3
    integer :: a
    integer :: b
  end type dt3
!$omp declare reduction(foo:dt1,dt2:omp_out%a=omp_out%a+omp_in%a)
!$omp declare reduction(foo:dt3:omp_out%a=omp_out%a+omp_in%a) ! { dg-error "Missing INITIALIZER clause for !.OMP DECLARE REDUCTION of derived type without default initializer" }
end subroutine f7
