! { dg-do compile }
! PR 94347 - this used to cause an ICE.
! Original test case by "Serghei".
program main
    character(10), target :: a
    character(:), pointer :: p => null()
    p => a
end program main
