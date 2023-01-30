! { dg-do compile }
! PR fortran/108420
! Contributed by G.Steinmetz

program p
  character :: c = 'c'
  logical   :: m = .true.
  print *, merge(transfer('a', 'b', 0), c, .true.)
  print *, merge(transfer('a', 'b', 0), c, m)
end
