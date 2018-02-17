!{ dg-do run }
! PR26464 File I/O error related to buffering and BACKSPACE
! Test case derived from case by Dale Ranta.
! Submitted  by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      program test
      integer,parameter :: datasize = 1000
      dimension idata(datasize)
      idata = -42
      open (11, status="scratch", form="unformatted")
        idata(1)   =  -1
        idata(  datasize)   =  -2
       write(11)idata
        idata(1)   =  -2
        idata(  datasize)   =  -3
       write(11)idata
        idata(1)   =  -3
        idata(  datasize)   =  -4
       write(11)idata
        idata(1)   =  -4
        idata(  datasize)   =  -5
       write(11)idata
       read(11,end=        1000 )idata
       STOP 1
 1000  continue
       backspace 11
       backspace 11
       backspace 11
       read(11,end=        1001 )idata
       if(idata(1).ne.-3 .or. idata(datasize).ne.-4) STOP 2
       stop
 1001  continue
       STOP 3
 1010  stop
       end

