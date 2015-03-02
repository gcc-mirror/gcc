! { dg-do run }
! PR 30056 - exceeding the record length was misrepresented as an EOF
!            on read and ignored on write 
      program main
      integer i,j
      open (10, form="unformatted", access="direct", recl=4)
      write (10, rec=1, err=10) 1,2
      call abort()
 10   continue
      read (10,  rec=1, err=20) i, j
      call abort()
 20   continue
      close (10, status="delete")
      end
