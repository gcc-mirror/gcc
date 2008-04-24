! { dg-do run }
! PR35994 [4.3/4.4 regression] MAXLOC and MINLOC off by one with mask
program GA4076
  REAL DDA(100)
  dda = (/(J1,J1=1,100)/)
  IDS = MAXLOC(DDA,1)
  if (ids.ne.100) call abort  !expect 100
  
  IDS = MAXLOC(DDA,1, (/(J1,J1=1,100)/) > 50)
  if (ids.ne.100) call abort  !expect 100

  IDS = minLOC(DDA,1)
  if (ids.ne.1) call abort  !expect 1

  IDS = MinLOC(DDA,1, (/(J1,J1=1,100)/) > 50)
  if (ids.ne.51) call abort !expect 51

END
