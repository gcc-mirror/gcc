! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
subroutine foo
   character(:), allocatable :: x[:]
   associate (y => x(:)(2:)) ! { dg-error "Rank mismatch|deferred type parameter" }
   end associate
end

subroutine bar
   character(:), allocatable :: x[:]
   associate (y => x(:)(:)) ! { dg-error "Rank mismatch|deferred type parameter" }
   end associate
end