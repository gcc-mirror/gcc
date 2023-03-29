! { dg-do compile }
! PR fortran/95375 - ICE in add_use_op
! Contributed by G.Steinmetz

function f() result(n) bind(c)      ! { dg-error "not C interoperable" }
  class(*), allocatable :: n
end
program p
  interface
     function f() result(n) bind(c)
       integer :: n
     end
  end interface
  if ( f() /= 0 ) stop
end

! { dg-prune-output "Type mismatch" }
