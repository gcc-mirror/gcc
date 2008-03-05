!{ dg-do run { target fd_truncate } }
! PR26464 File I/O error related to buffering and BACKSPACE
! Test case derived from case by Dale Ranta.
! Submitted  by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      program test
      integer,parameter :: datasize = 5000
      dimension idata(datasize)
      idata = -42
      open (11, status="scratch", form="unformatted")
        idata(1)   =  -1
        idata(datasize)   =  -2
       write(11)idata
        idata(1)   =  -2
        idata(datasize)   =  -3
       write(11)idata
        idata(1)   =  -3
        idata(datasize)   =  -4
       write(11)idata
       backspace 11
       backspace 11
        idata(1)   =  -2
        idata(datasize)   =  -3
       write(11)idata
       read(11,end=        1003 )idata
       call abort()
 1003  continue
       backspace 11
       backspace 11
       read(11,end=        1004 )idata
       if(idata(1).ne.-2 .or.idata(datasize).ne.-3) call abort()
       stop
 1004  continue
       end
       
