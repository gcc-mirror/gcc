! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

subroutine one
  !$omp teams num_teams(1) num_teams(2)  ! { dg-error "Duplicated 'num_teams' clause" }
  block; end block

  !$omp teams thread_limit(1) thread_limit(2)  ! { dg-error "Duplicated 'thread_limit' clause" }
  block; end block

  !$omp parallel num_threads(1) num_threads(2)  ! { dg-error "Duplicated 'num_threads' clause" }
  block; end block


  !$omp teams num_teams(dims([1,1]) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams num_teams(dims(1.0) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams num_teams(dims(i) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams num_teams(dims(i) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams num_teams(dims( 1 - 2) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams num_teams(dims( int ( cos(0.0) )) : 1)  ! OK
  block; end block


  !$omp teams thread_limit(dims([1,1]) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams thread_limit(dims(1.0) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams thread_limit(dims(i) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams thread_limit(dims(i) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams thread_limit(dims( 1 - 2) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp teams thread_limit(dims( int ( cos(0.0) )) : 1)  ! OK
  block; end block


  !$omp parallel num_threads(dims([1,1]) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp parallel num_threads(dims(1.0) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp parallel num_threads(dims(i) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp parallel num_threads(dims(i) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp parallel num_threads(dims( 1 - 2) : 1)  ! { dg-error "DIMS must be a constant positive integer" }
  block; end block

  !$omp parallel num_threads(dims( int ( cos(0.0) )) : 1)  ! OK
  block; end block
end

subroutine two
  !$omp parallel num_threads(dims( int ( cos(0.0) ) + 1 ) : 1)  ! { dg-error "The number of arguments \\(1\\) must be the same as specified for DIMS" }
  block; end block

  !$omp teams num_teams(dims( 3 )  : 1, 2, 3 , 4)  ! { dg-error "The number of arguments \\(4\\) must be the same as specified for DIMS" }
  block; end block

  !$omp teams thread_limit(dims( 3 ) : 1, 4)  ! { dg-error "The number of arguments \\(2\\) must be the same as specified for DIMS" }
  block; end block
end

subroutine three
  !$omp teams num_teams  ! { dg-error "Expected '\\(' after 'num_teams'" }
  block; end block

  !$omp teams num_teams( :  ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list'" }
  block; end block

  !$omp teams num_teams(  ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list'" }
  block; end block

  !$omp teams num_teams( dims ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list' at .1." }
  block; end block

  !$omp teams num_teams( dims(1) ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list'" }
  block; end block

  !$omp teams num_teams( dims(1) : ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list'" }
  block; end block

  !$omp teams num_teams( dims(1) : 1 ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list'" }
  block; end block

  !$omp teams num_teams( 5 : ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list'" }
  block; end block

  !$omp teams num_teams( 5 : 5 ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list' at .1." }
  block; end block
end

subroutine four
  !$omp teams thread_limit  ! { dg-error "Expected '\\(' after 'thread_limit'" }
  block; end block

  !$omp teams thread_limit( :  ! { dg-error "Expected STRICT, RELAXED or DIMS modifier" }
  block; end block

  !$omp teams thread_limit(  ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp teams thread_limit( dims ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp teams thread_limit( dims(1) ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp teams thread_limit( dims(1) : ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp teams thread_limit( dims(1) : 1 ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp teams thread_limit( 5 : ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp teams thread_limit( 5 : 5 ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block
end

subroutine five
  !$omp parallel num_threads  ! { dg-error "Expected '\\(' after 'num_threads'" }
  block; end block

  !$omp parallel num_threads( :  ! { dg-error "Expected STRICT, RELAXED or DIMS modifier" }
  block; end block

  !$omp parallel num_threads(  ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp parallel num_threads( dims ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp parallel num_threads( dims(1) ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp parallel num_threads( dims(1) : ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp parallel num_threads( dims(1) : 1 ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp parallel num_threads( 5 : ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block

  !$omp parallel num_threads( 5 : 5 ! { dg-error "Expected a list of integer expressions followed by a '\\)' and optionally preceded by the STRICT, RELAXED, or DIMS as modifiers and a colon at .1." }
  block; end block
end

subroutine six
  !$omp teams num_teams (dims(1), dims(1) : 1) ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list'" }
  block; end block


  !$omp teams thread_limit (dims(1), dims(1) : 1) ! { dg-error "Duplicated DIMS expression" }
  block; end block

  !$omp teams thread_limit (dims(1), relaxed, dims(1) : 1) ! { dg-error "Duplicated DIMS expression" }
  block; end block

  !$omp teams thread_limit (dims(1), strict, dims(1) : 1) ! { dg-error "Duplicated DIMS expression" }
  block; end block

  !$omp teams thread_limit (strict, relaxed, dims(1) : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp teams thread_limit (relaxed, strict, dims(1) : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp teams thread_limit (strict, relaxed : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp teams thread_limit (relaxed, strict : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp teams thread_limit (strict, dims(1), strict : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp teams thread_limit (relaxed, dims(1), relaxed : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp teams thread_limit (strict, strict : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp teams thread_limit (relaxed, relaxed : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block


  !$omp parallel num_threads (dims(1), dims(1) : 1) ! { dg-error "Duplicated DIMS expression" }
  block; end block

  !$omp parallel num_threads (dims(1), relaxed, dims(1) : 1) ! { dg-error "Duplicated DIMS expression" }
  block; end block

  !$omp parallel num_threads (dims(1), strict, dims(1) : 1) ! { dg-error "Duplicated DIMS expression" }
  block; end block

  !$omp parallel num_threads (strict, relaxed, dims(1) : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp parallel num_threads (relaxed, strict, dims(1) : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp parallel num_threads (strict, relaxed : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp parallel num_threads (relaxed, strict : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp parallel num_threads (strict, dims(1), strict : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp parallel num_threads (relaxed, dims(1), relaxed : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp parallel num_threads (strict, strict : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block

  !$omp parallel num_threads (relaxed, relaxed : 1) ! { dg-error "Only one STRICT or RELAXED modifier permitted" }
  block; end block
end subroutine

subroutine seven
  !$omp teams num_teams (2,2)  ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list'" }
  block; end block

  !$omp teams thread_limit (2,2)  ! { dg-error "Without the DIM modifier, only a single integer expression may be specified" }
  block; end block

  !$omp parallel num_threads (2,2)  ! OK
  block; end block

  !$omp teams num_teams(39 ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list' at .1." }
  block; end block
 
  !$omp teams num_teams(strict : 4 : 5 )  ! { dg-error "Expected either '\\\[lower-expr : \\\] upper-expr' or 'dims\\(N\\): expr-list' at .1." }
  block; end block
end
