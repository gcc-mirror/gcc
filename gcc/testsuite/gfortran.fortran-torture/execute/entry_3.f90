	subroutine f1 (n, *, i)
	integer n, i
	if (i .ne. 42) call abort ()
	entry e1 (n, *)
	if (n .eq. 1) return 1
	if (n .eq. 2) return
	return
	entry e2 (n, i, *, *, *)
	if (i .ne. 46) call abort ()
	if (n .ge. 4) return
	return n
	entry e3 (n, i)
	if ((i .ne. 48) .or. (n .ne. 61)) call abort ()
	end subroutine

	program alt_return
	implicit none

	call f1 (1, *10, 42)
20	continue
	call abort ()
10	continue
	call f1 (2, *20, 42)
	call f1 (3, *20, 42)
	call e1 (2, *20)
	call e1 (1, *30)
	call abort ()
30	continue
	call e2 (1, 46, *40, *20, *20)
	call abort ()
40	continue
	call e2 (2, 46, *20, *50, *20)
	call abort ()
50	continue
	call e2 (3, 46, *20, *20, *60)
	call abort ()
60	continue
	call e2 (4, 46, *20, *20, *20)
	call e3 (61, 48)
	end program
