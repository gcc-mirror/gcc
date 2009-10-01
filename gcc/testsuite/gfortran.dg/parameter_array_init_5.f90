! { dg-do run }
!
! PR fortran/41515
! Contributed by ros@rzg.mpg.de.
!
! Before, the "parm' string array was never initialized.
!
Module BUG3
contains
    Subroutine SR
    character(3)   :: parm(5)
    character(20)  :: str
    parameter(parm=(/'xo ','yo ','ag ','xr ','yr '/))

    str =    'XXXXXXXXXXXXXXXXXXXX'
    if(str /='XXXXXXXXXXXXXXXXXXXX') call abort()
    write(str,*) parm
    if(str /= ' xo yo ag xr yr') call abort()
    end subroutine SR
end Module BUG3
!
program TEST
    use bug3
    call sr
end program TEST
! { dg-final { cleanup-modules "bug3" } }
