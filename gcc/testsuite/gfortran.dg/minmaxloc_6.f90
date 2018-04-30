! { dg-do run }
! PR35994 [4.3/4.4 regression] MAXLOC and MINLOC off by one with mask
  REAL DDA(5:104)
  dda = (/(J1,J1=1,100)/)

  IDS = MAXLOC(DDA,1)
  if (ids.ne.100) STOP 1!expect 100
  IDS = MAXLOC(DDA,1, (/(J1,J1=1,100)/) > 50)
  if (ids.ne.100) STOP 2!expect 100

  END
