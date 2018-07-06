! { dg-do run }
! PR 26499  Test write with rewind sequences to make sure buffering and
! end-of-file conditions are handled correctly.  Derived from test case by Dale
! Ranta.  Submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>.
      program test
      dimension idata(1011)
      idata = -42
      open(unit=11,form='unformatted')
        idata(1)   = -705
        idata(  1011)   = -706
       write(11)idata
        idata(1)   = -706
        idata(  1011)   = -707
       write(11)idata
        idata(1)   = -707
        idata(  1011)   = -708
       write(11)idata
       read(11,end=        1000 )idata
       STOP 1
 1000  continue
       rewind 11
       read(11,end=        1001 )idata
        if(idata(1).ne. -705.or.idata(  1011).ne. -706)STOP 2
 1001  continue
       close(11,status='keep')        
      open(unit=11,form='unformatted')
      rewind 11
      read(11)idata
      if(idata(1).ne.-705)then
      STOP 3
      endif
      read(11)idata
      if(idata(1).ne.-706)then
      STOP 4
      endif
      read(11)idata
      if(idata(1).ne.-707)then
      STOP 5
      endif
      close(11,status='delete')  
      stop
      end


