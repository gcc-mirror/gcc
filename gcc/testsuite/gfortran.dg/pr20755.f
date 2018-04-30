! PR libfortran/20755
! { dg-do run }
! { dg-options "-std=legacy" }
!
      character*30 s
      
      write (s,2000) 0.0, 0.02
      if (s .ne. "    0.00       2.000E-02") STOP 1
      write (s,2000) 0.01, 0.02
      if (s .ne. "   1.000E-02   2.000E-02") STOP 2
 2000 format (1PG12.3,G12.3)
      end
