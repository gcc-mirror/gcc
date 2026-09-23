! { dg-do run }

program check
    implicit none
    integer :: u
    character(50) :: chr

    ! 1. Correctly generate a negative unit via NEWUNIT
    open(file="test.txt", newunit=u) ! u gets -10
    close(u) ! u (-10) is now closed and free

    ! 2. Internal I/O corrupts the state of unit -10
    write(chr, *) "test"

    ! 3. Redundant CLOSE on the valid unit 'u'.
    ! This should be a safe no-op according to the standard,
    ! but it triggers the runtime error/segfault!
    close(u)
end program
