! { dg-do  run }
! { dg-additional-options "-ffrontend-optimize -fdump-tree-original" }
! PR fortran/35339  - make sure that I/O of an implied DO loop
! of allocatable character arrays a) works and b) is converted
! to a transfer_array
program main
    implicit none
    integer:: i
    integer, parameter:: N = 10
    character(len=:), dimension(:),allocatable:: ca
    allocate(character(len=N):: ca(3))
    open(unit=10,status="scratch")
    ca(1) = "foo"
    ca(2) = "bar"
    ca(3) = "xyzzy"
    write (10, '(3A10)') (ca(i),i=1,3)
    rewind (10)
    ca(:) = ''
    read (10, '(3A10)') (ca(i),i=1,3)
    if (ca(1) /= 'foo' .or. ca(2) /= 'bar' .or. ca(3) /= 'xyzzy') call abort
end program
! { dg-final { scan-tree-dump-times "_gfortran_transfer_array" 2 "original" } }
