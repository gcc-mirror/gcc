! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR20866 - A statement function cannot be recursive.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
! Modified 20051110 to check that regressions PR24655 and PR24755
! are fixed. Thanks to pavarini@pv.infn.it and tdeutsch@cea.fr for
! the tests.
!
  INTEGER :: i, st1, st2, st3, lambda, n
  REAL :: x, z(2,2)
  character(8) :: ch
  real(8)   :: fi, arg, sigma, dshpfunc
  real(8), parameter :: one=1d0
!
! Test check for recursion via other statement functions, string
! length references, function actual arguments and array index
! references.
!
  st1 (i) = len (ch(st2 (1):8))
  st2 (i) = max (st3 (1), 4)
  st3 (i) = 2 + cos (z(st1 (1), i)) ! { dg-error "is recursive" }
!
! Test the two regressions.
!
  fi (n) = n *one
  dshpfunc (arg)=-lambda/sigma*(arg/sigma)**(lambda-1)*exp(-(arg/sigma)**lambda)
!
! References to each statement function.
!
  write(6,*) st1 (1), fi (2), dshpfunc (1.0_8)
  END
