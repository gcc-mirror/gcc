! { dg-do run }
! PR55818 Reading a REAL from a file which doesn't end in a new line fails
! Test case from PR reporter.
implicit none
integer :: stat
!integer :: var ! << works
real    :: var ! << fails
character(len=10)    :: cvar ! << fails
complex :: cval
logical :: lvar

open(99, file="test.dat", access="stream", form="unformatted", status="new")
write(99) "1", new_line("")
write(99) "2", new_line("")
write(99) "3"
close(99)

! Test character kind
open(99, file="test.dat")
read (99,*, iostat=stat) cvar
if (stat /= 0 .or. cvar /= "1") STOP 1
read (99,*, iostat=stat) cvar
if (stat /= 0 .or. cvar /= "2") STOP 2
read (99,*, iostat=stat) cvar              ! << FAILS: stat /= 0
if (stat /= 0 .or. cvar /= "3") STOP 3 ! << aborts here

! Test real kind
rewind(99)
read (99,*, iostat=stat) var
if (stat /= 0 .or. var /= 1.0) STOP 4
read (99,*, iostat=stat) var
if (stat /= 0 .or. var /= 2.0) STOP 5
read (99,*, iostat=stat) var ! << FAILS: stat /= 0
if (stat /= 0 .or. var /= 3.0) STOP 6
close(99, status="delete")

! Test real kind with exponents
open(99, file="test.dat", access="stream", form="unformatted", status="new")
write(99) "1.0e3", new_line("")
write(99) "2.0e-03", new_line("")
write(99) "3.0e2"
close(99)

open(99, file="test.dat")
read (99,*, iostat=stat) var
if (stat /= 0) STOP 7
read (99,*, iostat=stat) var
if (stat /= 0) STOP 8
read (99,*) var ! << FAILS: stat /= 0
if (stat /= 0) STOP 9
close(99, status="delete")

! Test logical kind
open(99, file="test.dat", access="stream", form="unformatted", status="new")
write(99) "Tru", new_line("")
write(99) "fal", new_line("")
write(99) "t"
close(99)

open(99, file="test.dat")
read (99,*, iostat=stat) lvar
if (stat /= 0 .or. (.not.lvar)) STOP 10
read (99,*, iostat=stat) lvar
if (stat /= 0 .or. lvar) STOP 11
read (99,*) lvar ! << FAILS: stat /= 0
if (stat /= 0 .or. (.not.lvar)) STOP 12
close(99, status="delete")

! Test combinations of Inf and Nan
open(99, file="test.dat", access="stream", form="unformatted", status="new")
write(99) "infinity", new_line("")
write(99) "nan", new_line("")
write(99) "infinity"
close(99)

open(99, file="test.dat")
read (99,*, iostat=stat) var
if (stat /= 0) STOP 13
read (99,*, iostat=stat) var
if (stat /= 0) STOP 14
read (99,*) var          ! << FAILS: stat /= 0
if (stat /= 0) STOP 1! << aborts here
close(99, status="delete")

open(99, file="test.dat", access="stream", form="unformatted", status="new")
write(99) "infinity", new_line("")
write(99) "inf", new_line("")
write(99) "nan"
close(99)

open(99, file="test.dat")
read (99,*, iostat=stat) var
if (stat /= 0) STOP 15
read (99,*, iostat=stat) var
if (stat /= 0) STOP 16
read (99,*) var          ! << FAILS: stat /= 0
if (stat /= 0) STOP 2! << aborts here
close(99, status="delete")

open(99, file="test.dat", access="stream", form="unformatted", status="new")
write(99) "infinity", new_line("")
write(99) "nan", new_line("")
write(99) "inf"
close(99)

open(99, file="test.dat")
read (99,*, iostat=stat) var
if (stat /= 0) STOP 17
read (99,*, iostat=stat) var
if (stat /= 0) STOP 18
read (99,*) var          ! << FAILS: stat /= 0
if (stat /= 0) STOP 3! << aborts here
close(99, status="delete")

! Test complex kind
open(99, file="test.dat", access="stream", form="unformatted", status="new")
write(99) "(1,2)", new_line("")
write(99) "(2,3)", new_line("")
write(99) "(4,5)"
close(99)

open(99, file="test.dat")
read (99,*, iostat=stat) cval
if (stat /= 0 .or. cval /= cmplx(1,2)) STOP 19
read (99,*, iostat=stat) cval
if (stat /= 0 .or. cval /= cmplx(2,3)) STOP 20
read (99,*, iostat=stat) cval      ! << FAILS: stat /= 0, value is okay
if (stat /= 0 .or. cval /= cmplx(4,5)) STOP 21
close(99, status="delete")
end
