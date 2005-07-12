! { dg-do run }
! This test currently only runs on systems where using ftruncate on
!   /dev/null fails (errno set to EINVAL). See PR 21593 for details.
!
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
