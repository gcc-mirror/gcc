! { dg-do run }
! PR 27575 : This test checks the error checking for end of file condition.
! Derived from test case in PR.
! Submitted by Jerry DeLisle <jvdelisle@verizon.net>.
      program test
      integer i1,i2,i3
      open(unit=11,form='unformatted')
      write(11)i1, i2     
      close(11,status='keep')
      open(unit=11,form='unformatted')
      read(11, eND=100) i1, i2, i3
      call abort()
 100  read(11, end=110) i3
      call abort() 
 110  close(11,status='delete')
      end
