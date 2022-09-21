! { dg-do compile }
! { dg-ice "Statements following a block in a metadirective" }
! PR fortran/107067

program metadirectives
   implicit none
   logical :: UseDevice

   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : nothing ) &
   !$OMP   default ( parallel )
   block
      call foo()
   end block
   call bar()   ! FIXME/XFAIL ICE in parse_omp_metadirective_body()
   !$omp end metadirective


   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : nothing ) &
   !$OMP   default ( parallel )
   block
      call bar()
   end block
   block        ! FIXME/XFAIL ICE in parse_omp_metadirective_body()
      call foo()
   end block
   !$omp end metadirective
end program


