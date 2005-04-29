! Test alternate entry points for functions when the result types
! of all entry points match

	function f1 (str, i, j) result (r)
	character str*(*), r1*(*), r2*(*), r*(*)
	integer i, j
	r = str (i:j)
	return
	entry e1 (str, i, j) result (r1)
	i = i + 1
	entry e2 (str, i, j) result (r2)
	j = j - 1
	r2 = str (i:j)
	end function

	function f3 () result (r)
	character r3*5, r4*5, r*5
	integer i
	r = 'ABCDE'
	return
	entry e3 (i) result (r3)
	entry e4 (i) result (r4)
	if (i .gt. 0) then
	  r3 = 'abcde'
	else
	  r4 = 'UVWXY'
	endif
	end function

	program entrytest
	character f1*16, e1*16, e2*16, str*16, ret*16
	character f3*5, e3*5, e4*5
	integer i, j
	str = 'ABCDEFGHIJ'
	i = 2
	j = 6
	ret = f1 (str, i, j)
	if ((i .ne. 2) .or. (j .ne. 6)) call abort ()
	if (ret .ne. 'BCDEF') call abort ()
	ret = e1 (str, i, j)
	if ((i .ne. 3) .or. (j .ne. 5)) call abort ()
	if (ret .ne. 'CDE') call abort ()
	ret = e2 (str, i, j)
	if ((i .ne. 3) .or. (j .ne. 4)) call abort ()
	if (ret .ne. 'CD') call abort ()
	if (f3 () .ne. 'ABCDE') call abort ()
	if (e3 (1) .ne. 'abcde') call abort ()
	if (e4 (1) .ne. 'abcde') call abort ()
	if (e3 (0) .ne. 'UVWXY') call abort ()
	if (e4 (0) .ne. 'UVWXY') call abort ()
	end program
