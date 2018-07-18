! { dg-do run }
! { dg-options "-fcray-pointer" }

! Test the fix for a runtime error 
! Contributed by Mike Kumbera <kumbera1@llnl.gov>

        program bob
        implicit none
        integer*8 ipfoo
        integer n,m,i,j
        real*8 foo
        
        common /ipdata/ ipfoo
        common /ipsize/ n,m
        POINTER ( ipfoo, foo(3,7) )

        n=3
        m=7

        ipfoo=malloc(8*n*m)
        do i=1,n
            do j=1,m
                foo(i,j)=1.d0
            end do
        end do
        call use_foo()
        end  program bob


        subroutine use_foo()
        implicit none
        integer n,m,i,j
        integer*8 ipfoo
        common /ipdata/ ipfoo
        common /ipsize/ n,m
        real*8 foo,boo

        !fails if * is the last dimension
        POINTER ( ipfoo, foo(n,*) )

        !works if the last dimension is specified
        !POINTER ( ipfoo, foo(n,m) )
        boo=0.d0
        do i=1,n
            do j=1,m
               boo=foo(i,j)+1.0
               if (abs (boo - 2.0) .gt. 1e-6) STOP 1
            end do
        end do

        end subroutine use_foo
