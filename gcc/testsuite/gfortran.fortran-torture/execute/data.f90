        ! Program to test data statement
        program data
        call sub1()
        call sub2()
        end
        subroutine sub1()
        integer i
        type tmp
          integer, dimension(4)::a 
          real :: r 
        end type
        type tmp1
          type (tmp) t1(4)
          integer b
        end type
        type (tmp1) tmp2(2)
        ! Full array and scalar component initializer
        data tmp2(2)%t1(2)%r, tmp2(1)%t1(3)%a, tmp2(1)%b/220,136,137,138,139,10/
        data tmp2(2)%t1(4)%a,tmp2(2)%t1(3)%a/241,242,4*5,233,234/
        ! implied DO
        data (tmp2(1)%t1(2)%a(i),i=4,1,-1)/124,123,122,121/
        ! array section
        data tmp2(1)%t1(4)%a(4:1:-1)/144,143,142,141/
        data tmp2(1)%t1(1)%a(1:4:2)/111,113/
        ! array element reference 
        data tmp2(2)%t1(2)%a(3), tmp2(2)%t1(2)%a(1)/223,221/

        if (any(tmp2(1)%t1(1)%a .ne. (/111,0,113,0/))) call abort
        if (tmp2(1)%t1(1)%r .ne. 0.0) call abort
        if (tmp2(1)%b .ne. 10) call abort

        if (any(tmp2(1)%t1(2)%a .ne. (/121,122,123,124/))) call abort
        if (tmp2(1)%t1(2)%r .ne. 0.0) call abort
        if (tmp2(1)%b .ne. 10) call abort

        if (any(tmp2(1)%t1(3)%a .ne. (/136,137,138,139/))) call abort
        if (tmp2(1)%t1(3)%r .ne. 0.0) call abort
        if (tmp2(1)%b .ne. 10) call abort

        if (any(tmp2(1)%t1(4)%a .ne. (/141,142,143,144/))) call abort
        if (tmp2(1)%t1(4)%r .ne. 0.0) call abort
        if (tmp2(1)%b .ne. 10) call abort

        if (any(tmp2(2)%t1(1)%a .ne. (/0,0,0,0/))) call abort
        if (tmp2(2)%t1(1)%r .ne. 0.0) call abort
        if (tmp2(2)%b .ne. 0) call abort

        if (any(tmp2(2)%t1(2)%a .ne. (/221,0,223,0/))) call abort
        if (tmp2(2)%t1(2)%r .ne. 220.0) call abort
        if (tmp2(2)%b .ne. 0) call abort

        if (any(tmp2(2)%t1(3)%a .ne. (/5,5,233,234/))) call abort
        if (tmp2(2)%t1(3)%r .ne. 0.0) call abort
        if (tmp2(2)%b .ne. 0) call abort

        if (any(tmp2(2)%t1(4)%a .ne. (/241,242,5,5/))) call abort
        if (tmp2(2)%t1(4)%r .ne. 0.0) call abort
        if (tmp2(2)%b .ne. 0) call abort

        end
        subroutine sub2()
        integer a(4,4), b(10)
        integer i,j,k
        real r,t
        data i,j,r,k,t,b(5),b(2),((a(i,j),i=1,4,1),j=4,1,-1)/1,2,3,4,5,5,2,&
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/
        if ((i.ne.1) .and. (j.ne.2).and.(k.ne.4)) call abort
        if ((r.ne.3.0).and.(t.ne.5.0))  call abort
        if (any(b.ne.(/0,2,0,0,5,0,0,0,0,0/))) call abort
        if (any(a.ne.reshape((/13,14,15,16,9,10,11,12,5,6,7,8,1,2,3,4/),(/4,4/)))) call abort
        end

