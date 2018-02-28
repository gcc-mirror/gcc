! { dg-do run { target fd_truncate } }
! PR34974 null bytes when reverse-tabbing long records
! Test case prepared by Jerry DeLisle <jvdelisle@gcc.gnu.org>
       program test
       character(1) :: a, b, c
       write (10,'(t50000,a,t1,a)') 'b', 'a'
       close (10)
       open (10, access="stream")
       read (10, pos=1) a
       read (10, pos=50000) b
       read (10, pos=25474) c
       close (10, status="delete")
       if (a /= "a") STOP 1
       if (b /= "b") STOP 2
       if (c /= " ") STOP 3
       end
