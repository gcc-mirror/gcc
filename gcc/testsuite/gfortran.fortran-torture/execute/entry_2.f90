! Test alternate entry points for functions when the result types
! of all entry points match

	character*(*) function f1 (str, i, j)
	character str*(*), e1*(*), e2*(*)
	integer i, j
	f1 = str (i:j)
	return
	entry e1 (str, i, j)
	i = i + 1
	entry e2 (str, i, j)
	j = j - 1
	e2 = str (i:j)
	end function

	character*5 function f3 ()
	character e3*(*), e4*(*)
	integer i
	f3 = 'ABCDE'
	return
	entry e3 (i)
	entry e4 (i)
	if (i .gt. 0) then
	  e3 = 'abcde'
	else
	  e4 = 'UVWXY'
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
	if ((i .ne. 2) .or. (j .ne. 6)) STOP 1
	if (ret .ne. 'BCDEF') STOP 2
	ret = e1 (str, i, j)
	if ((i .ne. 3) .or. (j .ne. 5)) STOP 3
	if (ret .ne. 'CDE') STOP 4
	ret = e2 (str, i, j)
	if ((i .ne. 3) .or. (j .ne. 4)) STOP 5
	if (ret .ne. 'CD') STOP 6
	if (f3 () .ne. 'ABCDE') STOP 7
	if (e3 (1) .ne. 'abcde') STOP 8
	if (e4 (1) .ne. 'abcde') STOP 9
	if (e3 (0) .ne. 'UVWXY') STOP 10
	if (e4 (0) .ne. 'UVWXY') STOP 11
	end program
