! { dg-do run }
! PR 34980 - we got a segfault for calling shape
!            with a scalar.
program main
 integer :: n
 n = 5
 open(10,status="scratch")
 write (10,*) shape(n)
 close(10,status="delete")
end

