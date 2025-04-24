! { dg-do compile }

! Check duplicate clause detection after mapper expansion.

type t
integer :: x
end type t

real(4) :: unrelated
type(t) :: tvar

!$omp declare mapper (t :: var) map(unrelated) map(var%x)

tvar%x = 0
unrelated = 5

!$omp target firstprivate(unrelated) map(tofrom: tvar)
! { dg-error "Symbol .unrelated. present on both data and map clauses" "" { target *-*-* } .-1 }
tvar%x = unrelated
!$omp end target

end
