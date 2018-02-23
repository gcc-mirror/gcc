C { dg-do run }
C { dg-options "-fbounds-check" }
      character*25 buff(0:10)
      character*80 line
      integer i, m1, m2
      i  = 1
      m1 = 1
      m2 = 7
      buff(i) = 'tcase0a'
      write(line,*) buff(i)(m1:m2)
      if (line .ne. ' tcase0a') STOP 1
      end
