! { dg-do run }
!
! PR fortran/56735
!
! Contributed by Adam Williams
!
        PROGRAM TEST
        INTEGER int1,int2,int3
        NAMELIST /temp/ int1,int2,int3

        int1 = -1; int2 = -2; int3 = -3

        OPEN (53, STATUS='scratch')
        WRITE (53, '(a)') ' ?'
        WRITE (53, '(a)')
        WRITE (53, '(a)') '$temp'
        WRITE (53, '(a)') ' int1=1'
        WRITE (53, '(a)') ' int2=2'
        WRITE (53, '(a)') ' int3=3'
        WRITE (53, '(a)') '$END'
        REWIND(53)

        READ (53, temp)
        CLOSE (53)

        if (int1 /= 1 .or. int2 /= 2 .or. int3 /= 3) call abort()
        END PROGRAM
