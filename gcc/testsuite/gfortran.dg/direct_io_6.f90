!{ dg-do run }
! PR25289 Cannot handle record numbers larger than huge(0_4).
! This test checks that very large record numbers can be used.
! Derived from example in PR.
! Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
      integer(KIND=1) abyte
      integer(KIND=8) n
      n = huge(0_4)
      n = n * 256
      abyte = 105
      open(10,file="foo",recl=2,form='unformatted',access='direct')
      write(10,rec=n) abyte
      abyte = 0
      read(10,rec=n) abyte
      if (abyte.ne.105) call abort()
      write(10,rec=1) abyte
      abyte = 0
      read(10,rec=1) abyte
      if (abyte.ne.105) call abort()
      n=n/2
      write(10,rec=n) abyte
      abyte = 0
      read(10,rec=n) abyte
      if (abyte.ne.105) call abort()
      close(10, status="delete")
      end
      