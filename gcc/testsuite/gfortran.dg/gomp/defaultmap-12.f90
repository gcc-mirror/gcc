! PR fortran/129067

subroutine sub
  !$omp target defaultmap ( firstprivate : optional , allocatable )  ! { dg-message "sorry, unimplemented: 'optional' modifier is not supported yet" }
  !$omp end target

  !$omp target defaultmap(tofrom:optional,all)  ! { dg-message "sorry, unimplemented: 'optional' modifier is not supported yet" }
  !$omp end target

  !$omp target defaultmap (alloc : scalar , optional )  ! { dg-message "sorry, unimplemented: 'optional' modifier is not supported yet" }
  !$omp end target

  !$omp target defaultmap(present:aggregate,optional)  ! { dg-message "sorry, unimplemented: 'optional' modifier is not supported yet" }
  !$omp end target

  !$omp target defaultmap ( to : optional )  ! { dg-message "sorry, unimplemented: 'optional' modifier is not supported yet" }
  !$omp end target

  !$omp target defaultmap(from:optional)  ! { dg-message "sorry, unimplemented: 'optional' modifier is not supported yet" }
  !$omp end target

  !$omp target defaultmap(firstprivate : aggregate) defaultmap(tofrom : scalar) defaultmap(present : pointer) defaultmap(alloc ! { dg-error "Expected '\\)' or ':' at .1." }
  block; end block

  !$omp target defaultmap(firstprivate : aggregate) defaultmap(tofrom : scalar) defaultmap(present : pointer) defaultmap(alloc:  ! { dg-error "Expected SCALAR, AGGREGATE, ALLOCATABLE, POINTER, ALL, or OPTIONAL at .1." }
  block; end block

  !$omp target defaultmap(from:optional  ! { dg-error "Expected '\\)' or ',' at .1." }
  block; end block

  !$omp target defaultmap(from:optional,  ! { dg-error "Expected SCALAR, AGGREGATE, ALLOCATABLE, POINTER, or ALL at .1." }
  block; end block

  !$omp target defaultmap(from:optional,optional  ! { dg-error "Expected SCALAR, AGGREGATE, ALLOCATABLE, POINTER, or ALL at .1." }
  block; end block

  !$omp target defaultmap(from:aggregate  ! { dg-error "Expected '\\)' at .1." }
  block; end block

  !$omp target defaultmap(from:aggregate , ! { dg-error "Expected OPTIONAL at .1." }
  block; end block
end
