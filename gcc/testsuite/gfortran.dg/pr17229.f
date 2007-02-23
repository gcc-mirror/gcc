! PR fortran/17229
! { dg-do run }
      integer i
      logical l

      l = .false.
      i = -1
      if (l) if (i) 999,999,999 ! { dg-warning "Obsolescent: arithmetic IF statement" }

      l = .true.
      if (l) if (i) 10,999,999 ! { dg-warning "Obsolescent: arithmetic IF statement" }
      go to 999

   10 i = 0
      if (l) if (i) 999,20,999 ! { dg-warning "Obsolescent: arithmetic IF statement" }
      go to 999

   20 i = 1
      if (l) if (i) 999,999,30 ! { dg-warning "Obsolescent: arithmetic IF statement" }
      go to 999

  999 call abort
   30 end
