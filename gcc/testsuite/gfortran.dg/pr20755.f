! PR libfortran/20755
! { dg-do run }
      character*30 s
      
      write (s,2000) 0.0, 0.02
      if (s .ne. "    0.00       2.000E-02") call abort
      write (s,2000) 0.01, 0.02
      if (s .ne. "   1.000E-02   2.000E-02") call abort
 2000 format (1PG12.3,G12.3)
      end
