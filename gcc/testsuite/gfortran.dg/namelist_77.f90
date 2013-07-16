! { dg-do run }
!
! PR libfortran/51825 - Fortran runtime error: Cannot match namelist object name
! Test case derived from PR.

module local_mod

    type mytype1
        integer :: int1
    end type

    type mytype2
        integer :: n_x       
        integer :: n_px        
    end type

    type beam_init_struct
        character(16) :: chars(1) = ''                                  
        type (mytype1) dummy
        type (mytype2) grid(1)      
    end type

end module

program error_namelist

    use local_mod

    implicit none

    type (beam_init_struct) beam_init

    namelist / error_params / beam_init

    open (10, status='scratch')
    write (10, '(a)') "&error_params"
    write (10, '(a)') "  beam_init%chars(1)='JUNK'"
    write (10, '(a)') "  beam_init%grid(1)%n_x=3"
    write (10, '(a)') "  beam_init%grid(1)%n_px=2"
    write (10, '(a)') "/"
    rewind(10)
    read(10, nml=error_params)
    close (10)

    if (beam_init%chars(1) /= 'JUNK') call abort
    if (beam_init%grid(1)%n_x /= 3) call abort
    if (beam_init%grid(1)%n_px /= 2) call abort

end program
