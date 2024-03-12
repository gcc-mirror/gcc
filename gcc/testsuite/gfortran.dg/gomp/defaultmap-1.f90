! PR fortran/92568

implicit none

!$omp target defaultmap(bar)  ! { dg-error "25: Expected ALLOC, TO, FROM, TOFROM, FIRSTPRIVATE, PRESENT, NONE or DEFAULT" }

!$omp target defaultmap ( alloc: foo)  ! { dg-error "34: Expected SCALAR, AGGREGATE, ALLOCATABLE, POINTER or ALL" }

!$omp target defaultmap(alloc:scalar) defaultmap(none:Scalar)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category SCALAR" }

!$omp target defaultmap(default:aggregate) defaultmap(tofrom)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category AGGREGATE" }

!$omp target defaultmap(from:pointer) defaultmap(tofrom:pointer)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category POINTER" }

!$omp target defaultmap(from:scalar) defaultmap(to:allocatable) defaultmap(tofrom:allocatable)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category ALLOCATABLE" }

!$omp target defaultmap(from) defaultmap(to)  !  { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP with unspecified category" }

end
