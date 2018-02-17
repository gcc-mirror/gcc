c { dg-do run }
        PROGRAM TEST
        REAL AB(3)
        do i=1,3
         AB(i)=i
        enddo
        k=1
        n=2
        ind=k-n+2
        if (ind /= 1) STOP 1
        if (ab(ind) /= 1) STOP 2
        if (k-n+2 /= 1) STOP 3
        if (ab(k-n+2) /= 1) STOP 4
        END
