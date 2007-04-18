! { dg-do run }
! pr31366 last record truncated for read after short write, direct access file.
! test case derived from pr, submitted by jerry delisle  <jvdelisle@gcc.gnu.org
      program test
      character(len=8) :: as_written, as_read
      character(1) :: byte
      as_written = "12345678"
      open (76, access="direct", recl=12, status="scratch")
      write(76, rec=1) as_written
      write(76, rec=2) as_written
      read(76, rec=1) as_read, byte, byte, byte, byte
      read(76, rec=2, err=3) as_read, byte, byte, byte, byte
      stop
  3   call abort()
      end program test

