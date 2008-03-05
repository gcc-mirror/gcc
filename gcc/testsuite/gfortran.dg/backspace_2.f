! { dg-do run { target fd_truncate } }
! PR25139 Repeated backspaces and reads.
! Derived from example given in PR by Dale Ranta and FX Coudert
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org> 
      integer dat(5)
      dat = (/ 0, 0, 0, 0, 1 /)
      write(11) dat,dat,dat,dat
      rewind 11
      write(11) dat
      read(11,end=1008) dat
      call abort()
 1008 continue
      backspace 11
      write(11) dat
      read(11,end=1011) dat
      call abort()
 1011 continue
      backspace 11
      backspace 11
      close(11, status='delete')
      end

