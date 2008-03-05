C { dg-do run { target fd_truncate } }
C PR libfortran/31618 - backspace after an error didn't work.
      program main
      character*78 msg
      open (21, file="backspace_7.dat", form="unformatted")
      write (21) 42, 43
      write (21) 4711, 4712
      write (21) -1, -4
      rewind (21)
      read (21) i,j
      read (21,err=100,end=100) i,j,k
      call abort
 100  continue
      backspace 21
      read (21) i,j
      if (i .ne. 4711 .or. j .ne. 4712) call abort
      close (21,status="delete")
      end
