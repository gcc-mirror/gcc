! { dg-do run }
! { dg-options "-O3" }
! From forall_12.f90
! Fails with loop reversal at -O3
!
  character(len=1) :: b(4) = (/"1","2","3","4"/), c(4)
  c = b
  i = 1
  ! This statement must be here for the abort below
  b(1:3)(i:i) = b(2:4)(i:i)

  b = c
  b(4:2:-1)(i:i) = b(3:1:-1)(i:i)

  ! This fails.  If the condition is printed, the result is F F F F
  if (any (b .ne. (/"1","1","2","3"/))) i = 2
  print *, b
  print *, b .ne. (/"1","1","2","3"/)
  if (i == 2) STOP 1
end
