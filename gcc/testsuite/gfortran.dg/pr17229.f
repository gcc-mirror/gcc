! PR fortran/17229
! { dg-do run }
! { dg-options "-std=legacy" }

      integer i
      logical l

      l = .false.
      i = -1
      if (l) if (i) 999,999,999

      l = .true.
      if (l) if (i) 10,999,999
      go to 999

   10 i = 0
      if (l) if (i) 999,20,999
      go to 999

   20 i = 1
      if (l) if (i) 999,999,30
      go to 999

  999 STOP 1
   30 end
