        PROGRAM TEST
        REAL AB(3)
        do i=1,3
         AB(i)=i
        enddo
        k=1
        n=2
        ind=k-n+2
	if (ind /= 1) call exit(1)
	if (ab(ind) /= 1) call exit(1)
	if (k-n+2 /= 1) call exit(1)
	if (ab(k-n+2) /= 1) call exit(1)
        END
