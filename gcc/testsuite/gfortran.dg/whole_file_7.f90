! { dg-do compile }
! { dg-options "-fwhole-file" }
! Test the fixes for the first two problems in PR40011
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>
!
! This function would not compile because -fwhole-file would
! try repeatedly to resolve the function because of the self
! reference.
RECURSIVE FUNCTION eval_args(q)  result (r)
  INTEGER NNODE 
  PARAMETER (NNODE  = 10) 
  TYPE NODE 
    SEQUENCE 
    INTEGER car 
    INTEGER cdr 
  END TYPE NODE 
  TYPE(NODE) heap(NNODE) 
  INTEGER r, q 
  r = eval_args(heap(q)%cdr) 
END FUNCTION eval_args 

function test(n)
  real, dimension(2) :: test
  integer            :: n
  test = n
  return
end function test

program arr     ! The error was not picked up causing an ICE
  real, dimension(2) :: res
  res = test(2) ! { dg-error "needs an explicit INTERFACE" }
  print *, res
end program
