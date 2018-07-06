	subroutine f1 (n, *, i)
	integer n, i
	if (i .ne. 42) STOP 1
	entry e1 (n, *)
	if (n .eq. 1) return 1
	if (n .eq. 2) return
	return
	entry e2 (n, i, *, *, *)
	if (i .ne. 46) STOP 2
	if (n .ge. 4) return
	return n
	entry e3 (n, i)
	if ((i .ne. 48) .or. (n .ne. 61)) STOP 3
	end subroutine

	program alt_return
	implicit none

	call f1 (1, *10, 42)
20	continue
	STOP 4
10	continue
	call f1 (2, *20, 42)
	call f1 (3, *20, 42)
	call e1 (2, *20)
	call e1 (1, *30)
	STOP 5
30	continue
	call e2 (1, 46, *40, *20, *20)
	STOP 6
40	continue
	call e2 (2, 46, *20, *50, *20)
	STOP 7
50	continue
	call e2 (3, 46, *20, *20, *60)
	STOP 8
60	continue
	call e2 (4, 46, *20, *20, *20)
	call e3 (61, 48)
	end program
