!{ dg-do run }
! PR26423 Large file I/O error related to buffering
! Test case derived from case by Dale Ranta.
! Submitted  by Jerry DeLisle  <jvdelisle@gcc.gnu.org> 
      integer :: a(3000) , b(2048)
      a=3
      b=5
      a(1) = 1
      a(3000)=1234
      write(2) a
      b(1) = 2
      b(2048) = 5678
      write(2) b
      rewind 2
      read(2) a
      read(2) b
      if (a(1).ne.1) STOP 1
      if (a(2).ne.3) STOP 2
      if (b(1).ne.2) STOP 3
      if (b(2).ne.5) STOP 4
      if (a(3000).ne.1234) STOP 5
      if (b(2048).ne.5678) STOP 6
      close(2, status='delete')
      end
