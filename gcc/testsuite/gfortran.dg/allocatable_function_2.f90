! { dg-do compile }
! Test constraints on ALLOCATABLE functions
program alloc_fun

contains

    elemental function foo (n)
        integer, intent(in) :: n
        integer, allocatable :: foo(:) ! { dg-error "ALLOCATABLE .* ELEMENTAL" }
    end function foo

end program alloc_fun
