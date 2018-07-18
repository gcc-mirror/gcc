! { dg-do run }
! PR 27575 and PR 30009: This test checks the error checking for end
! of file condition.
! Derived from test case in PR.
! Submitted by Jerry DeLisle <jvdelisle@verizon.net>, modified by
! Thomas Koenig <Thomas.Koenig@online.de>

      program test
      integer i1,i2,i3
      open(unit=11,form='unformatted')
      write (11) 1, 2
      write (11) 3, 4
      close(11,status='keep')

      open(unit=11,form='unformatted')

      read(11, ERR=100) i1, i2, i3
      STOP 1
  100 continue
      if (i1 /= 1 .or. i2 /= 2) STOP 1

      read(11, ERR=110) i1, i2, i3
      STOP 2
  110 continue
      if (i1 /= 3 .or. i2 /= 4) STOP 2

      read(11, end=120) i3
      STOP 3
 120  close(11,status='delete')
      end
