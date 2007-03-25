! { dg-do run }
! PR 26499 : Positioning of EOF after  write and rewind.
! Test case from Dale Ranta in PR.
! Submitted by Jerry DeLisle <jvdelisle@verizon.net>.
      program test
      dimension idata(100)
      idata = -42
      open(unit=11,form='unformatted')
      write(11)idata
      write(11)idata
      read(11,end=        1000 )idata
      call abort()
 1000 continue
      rewind 11
      write(11)idata
      close(11,status='keep')        
      open(unit=11,form='unformatted')
      rewind 11
      read(11)idata
      read(11, end=250)idata
      call abort()
 250  continue
      close(11,status='delete')  
      end
