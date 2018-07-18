! { dg-do run }
! Test that allocatable characters with deferred length
! are written correctly
program main
    implicit none
    integer:: i
    integer, parameter:: N = 10
    character(len=:), dimension(:),allocatable:: ca
    character(len=50):: buffer, line
    allocate(character(len=N):: ca(3))
    buffer = "foo  bar  xyzzy"
    ca(1) = "foo"
    ca(2) = "bar"
    ca(3) = "xyzzy"
    write (unit=line, fmt='(3A5)') (ca(i),i=1,3)
    if (line /= buffer) STOP 1
    ca(1) = ""
    ca(2) = ""
    ca(3) = ""
    read (unit=line, fmt='(3A5)') (ca(i),i=1,3)
    if (line /= buffer) STOP 2
end program

