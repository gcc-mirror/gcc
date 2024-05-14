! { dg-do compile }

program metadirectives
   implicit none
   logical :: UseDevice

   !$OMP metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : parallel ) &
   !$OMP   default ( parallel )
   block
      call bar()
   end block

   !$OMP metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : parallel ) &
   !$OMP   default ( parallel )
   call bar()
   !$omp end parallel  ! Accepted, because all cases have 'parallel'
   
   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : nothing ) &
   !$OMP   default ( parallel )
   call bar()
   block
      call foo()
   end block
   !$OMP end metadirective

   !$OMP begin metadirective &
   !$OMP   when ( user = { condition ( UseDevice ) } &
   !$OMP     : parallel ) &
   !$OMP   default ( parallel )
   call bar()
   !$omp end parallel  ! { dg-error "Unexpected !.OMP END PARALLEL statement at .1." }
end program ! { dg-error "Unexpected END statement at .1." }

! { dg-error "Unexpected end of file" "" { target *-*-* } 0 }
