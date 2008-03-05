c { dg-do run { target fd_truncate } }
c pr 19657
c test namelist not skipped if ending with logical.
c Based on example provided by fuyuki@ccsr.u-tokyo.ac.jp

      program pr19657
      implicit none
      logical   l
      integer   i, ctr
      namelist /nm/ i, l
      open (10, status = "scratch")
      write (10,*) "&nm i=1,l=t &end"
      write (10,*) "&nm i=2 &end"
      write (10,*) "&nm i=3 &end"
      rewind (10)
      do ctr = 1,3
        read (10,nm,end=190)
        if (i.ne.ctr) call abort ()
      enddo
 190  continue 
      end
