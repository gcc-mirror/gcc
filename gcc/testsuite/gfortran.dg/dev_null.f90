! { dg-do run }
! pr19478 read from /dev/null
! Thomas.Koenig@online.de
      character*20 foo
      open(10,file="/dev/null")
      write(10,'(A)') "Hello"
      rewind(10)
      read(10,'(A)',end=100) foo
      call abort
 100  continue
      end
