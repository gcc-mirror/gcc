! { dg-do run }
! PR32678 GFortan works incorrectly when writing with FORMAT Tx
! Before patch, NULLs were inserted in output.
! Test case from reporter enhanced to detect this problem.
      character(25) :: output
      character(1)  :: c
      output = ""
      open (unit=10, file="pr32678testfile", status="replace")
      write (10,10) '12','a','b'
      close (10, status="keep")
      open (unit=10, file="pr32678testfile", access="stream")
      read(10, pos=1) output(1:21)
      if (output(1:21).ne."ab                  x") STOP 1
      read(10) c
      if ((c.ne.achar(10)) .and. (c.ne.achar(13))) STOP 2
      close (10, status="delete")
 10   format (a2,t1,a1,t2,a1,t20,' x')
      end
