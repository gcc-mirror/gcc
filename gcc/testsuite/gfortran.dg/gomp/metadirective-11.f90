! { dg-do compile }
! PR fortran/107067

program metadirectives
   implicit none
   logical :: UseDevice
   integer :: n, v

   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : nothing ) &
   !$OMP   default ( parallel )  ! { dg-error "Variants in a metadirective at .1. have different associations" }
   block
      call foo()
   end block
   call bar()   ! { dg-error "Expected OMP END METADIRECTIVE" } 
   !$omp end metadirective ! { dg-error "Unexpected ..OMP END METADIRECTIVE statement" }

   ! It's a quirk of the implementation that gfortran thinks the metadirective
   ! ends where the *last* variant ends.  If we reverse the order of the
   ! variants from the previous case, the "unexpected OMP END METADIRECTIVE"
   ! error disappears because the "nothing" variant eats it where the
   ! "parallel" directive doesn't.

   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : parallel ) &
   !$OMP   default ( nothing )  ! { dg-error "Variants in a metadirective at .1. have different associations" }
   block
      call foo()
   end block
   call bar()   ! { dg-error "Expected OMP END METADIRECTIVE" } 
   !$omp end metadirective

   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : nothing ) &
   !$OMP   default ( parallel )  ! { dg-error "Variants in a metadirective at .1. have different associations" }
   block
      call bar()
   end block
   block        ! { dg-error "Expected OMP END METADIRECTIVE" } 
      call foo()
   end block
   !$omp end metadirective ! { dg-error "Unexpected ..OMP END METADIRECTIVE statement" }

   ! This one depends on the locus comparison and not just the statement
   ! code comparison to diagnose the "different associations" error, since
   ! there are two call statements involved.
   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : nothing ) &
   !$OMP   default ( parallel )  ! { dg-error "Variants in a metadirective at .1. have different associations" }
   block
      call foo()
   end block
   call bar()        ! { dg-error "Expected OMP END METADIRECTIVE" } 
   !$omp end metadirective ! { dg-error "Unexpected ..OMP END METADIRECTIVE statement" }
   call baz()

   ! The "nothing" directive in a non-begin/end metadirective only applies to a
   ! a single statement or block, while "atomic capture" permits multiple
   ! assignment statements.
   !$OMP metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : nothing ) &
   !$OMP   default (atomic capture)  ! { dg-error "Variants in a metadirective at .1. have different associations" }
   n = n + 1; v = n

   ! Reverse order of the above.
   !$OMP metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : atomic capture ) &
   !$OMP   default ( nothing )  ! { dg-error "Variants in a metadirective at .1. have different associations" }
   n = n + 1; v = n

   ! This one is correct because both variants are properly terminated
   ! by the "end metadirective".
   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : nothing ) &
   !$OMP   default (atomic capture)
   n = n + 1; v = n
   !$omp end metadirective

end program


