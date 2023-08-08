! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

type t
integer :: a, b, c, d
end type t

type(t) :: tvar

!$omp declare mapper (T :: t) map(alloc: t%a) map(to: t%b) map(from: t%c) &
!$omp & map(tofrom: t%d)

!$omp declare mapper (updatey: T :: t) map(t%a) map(t%b) map(t%c) map(t%d)

!$omp target update to(tvar)
! { dg-warning "Dropping incompatible .ALLOC. mapper clause" "" { target *-*-* } .-1 }
! { dg-warning "Dropping incompatible .FROM. mapper clause" "" { target *-*-* } .-2 }
! { dg-final { scan-tree-dump-times {(?n)update to\(tvar\.b \[len: [0-9]+\]\) to\(tvar\.d \[len: [0-9]+\]\)$} 1 "original" } }
!$omp target update from(tvar)
! { dg-warning "Dropping incompatible .ALLOC. mapper clause" "" { target *-*-* } .-1 }
! { dg-warning "Dropping incompatible .TO. mapper clause" "" { target *-*-* } .-2 }
! { dg-final { scan-tree-dump-times {(?n)update from\(tvar\.c \[len: [0-9]+\]\) from\(tvar\.d \[len: [0-9]+\]\)$} 1 "original" } }

!$omp target update to(present: tvar)
! { dg-warning "Dropping incompatible .ALLOC. mapper clause" "" { target *-*-* } .-1 }
! { dg-warning "Dropping incompatible .FROM. mapper clause" "" { target *-*-* } .-2 }
! { dg-final { scan-tree-dump-times {(?n)update to\(present:tvar\.b \[len: [0-9]+\]\) to\(present:tvar\.d \[len: [0-9]+\]\)$} 1 "original" } }
!$omp target update from(present: tvar)
! { dg-warning "Dropping incompatible .ALLOC. mapper clause" "" { target *-*-* } .-1 }
! { dg-warning "Dropping incompatible .TO. mapper clause" "" { target *-*-* } .-2 }
! { dg-final { scan-tree-dump-times {(?n)update from\(present:tvar\.c \[len: [0-9]+\]\) from\(present:tvar\.d \[len: [0-9]+\]\)$} 1 "original" } }

!$omp target update to(mapper(updatey): tvar)
! { dg-final { scan-tree-dump-times {(?n)update to\(tvar\.a \[len: [0-9]+\]\) to\(tvar\.b \[len: [0-9]+\]\) to\(tvar\.c \[len: [0-9]+\]\) to\(tvar\.d \[len: [0-9]+\]\)$} 1 "original" } }
!$omp target update from(mapper(updatey): tvar)
! { dg-final { scan-tree-dump-times {(?n)update from\(tvar\.a \[len: [0-9]+\]\) from\(tvar\.b \[len: [0-9]+\]\) from\(tvar\.c \[len: [0-9]+\]\) from\(tvar\.d \[len: [0-9]+\]\)$} 1 "original" } }

!$omp target update to(present, mapper(updatey): tvar)
! { dg-final { scan-tree-dump-times {(?n)update to\(present:tvar\.a \[len: [0-9]+\]\) to\(present:tvar\.b \[len: [0-9]+\]\) to\(present:tvar\.c \[len: [0-9]+\]\) to\(present:tvar\.d \[len: [0-9]+\]\)$} 1 "original" } }
!$omp target update from(present, mapper(updatey): tvar)
! { dg-final { scan-tree-dump-times {(?n)update from\(present:tvar\.a \[len: [0-9]+\]\) from\(present:tvar\.b \[len: [0-9]+\]\) from\(present:tvar\.c \[len: [0-9]+\]\) from\(present:tvar\.d \[len: [0-9]+\]\)$} 1 "original" } }

end
