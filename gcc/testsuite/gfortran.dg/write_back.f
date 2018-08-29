! { dg-do run { target fd_truncate } }
! PR 26499 : Positioning of EOF after backspaces and write.
! This test verifies that the last write truncates the file.
! Submitted by Jerry DeLisle <jvdelisle@verizon.net>.
      program test
      integer at,eof
      dimension idata(5)
      idata = -42
      open(unit=11,form='unformatted')
      write(11)idata
      write(11)idata
      write(11)idata
      backspace(11)
      backspace(11)
      write(11)idata
      close(11, status="keep")  
      open(unit=11,form='unformatted')
      rewind(11)
      read(11)idata
      read(11)idata
      read(11, end=250)idata
      STOP 1
 250  continue
      close(11, status="delete")
      end

