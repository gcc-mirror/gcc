! { dg-do run }
! PR48976 test case by jvdelisle@gcc.gnu.org
character(len=20) :: str
str = "abcdefg"
inquire(file="abcddummy", stream=str)
!print *, "str=",str
if (str /= "UNKNOWN") STOP 1
inquire(99, stream=str)
!print *, "str=",str
if (str /= "UNKNOWN") STOP 2
open(99,access="stream")
inquire(99, stream=str)
!print *, "str=",str
if (str /= "YES") goto 10
close(99)
open(99,access="direct", recl=16)
inquire(99, stream=str)
!print *, "str=",str
if (str /= "NO") goto 10
close(99)
open(99,access="sequential")
inquire(99, stream=str)
!print *, "str=",str
if (str /= "NO") goto 10
close(99, status="delete")
stop
10 close(99, status="delete")
STOP 3
end
