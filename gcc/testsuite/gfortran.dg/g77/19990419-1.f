c { dg-do run }
* Test DO WHILE, to make sure it fully reevaluates its expression.
* Belongs in execute/.
      common /x/ ival
      j = 0
      do while (i() .eq. 1)
         j = j + 1
         if (j .gt. 5) STOP 1
      end do
      if (j .ne. 4) STOP 2
      if (ival .ne. 5) STOP 3
      end
      function i()
      common /x/ ival
      ival = ival + 1
      i = 10
      if (ival .lt. 5) i = 1
      end
      block data
      common /x/ ival
      data ival/0/
      end
