! { dg-do run }
!
! PR libfortran/51825
! Test case regarding namelist problems with derived types

program namelist

    type d1
        integer :: j = 0
    end type d1

    type d2
        type(d1) k
    end type d2

    type d3
        type(d2) d(2)
    end type d3

    type(d3) der
    namelist /nmlst/ der

    open (10, status='scratch')
    write (10, '(a)') "&NMLST"
    write (10, '(a)') " DER%D(1)%K%J = 1,"
    write (10, '(a)') " DER%D(2)%K%J = 2,"
    write (10, '(a)') "/"
    rewind(10)
    read(10, nml=nmlst)
    close (10)

    if (der%d(1)%k%j /= 1) call abort
    if (der%d(2)%k%j /= 2) call abort
end program namelist
