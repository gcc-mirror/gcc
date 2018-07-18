! Test alternate entry points for functions when the result types
! of all entry points match

	function f1 (a)
	integer a, b, f1, e1
	f1 = 15 + a
	return
	entry e1 (b)
	e1 = 42 + b
	end function
	function f2 ()
	real f2, e2
	entry e2 ()
	e2 = 45
	end function
	function f3 ()
	double precision a, b, f3, e3
	entry e3 ()
	f3 = 47
	end function
	function f4 (a) result (r)
	double precision a, b, r, s
	r = 15 + a
	return
	entry e4 (b) result (s)
	s = 42 + b
	end function
	function f5 () result (r)
	integer r, s
	entry e5 () result (s)
	r = 45
	end function
	function f6 () result (r)
	real r, s
	entry e6 () result (s)
	s = 47
	end function
	function f7 ()
	entry e7 ()
	e7 = 163
	end function
	function f8 () result (r)
	entry e8 ()
	e8 = 115
	end function
	function f9 ()
	entry e9 () result (r)
	r = 119
	end function

	program entrytest
	integer f1, e1, f5, e5
	real f2, e2, f6, e6, f7, e7, f8, e8, f9, e9
	double precision f3, e3, f4, e4, d
	if (f1 (6) .ne. 21) STOP 1
	if (e1 (7) .ne. 49) STOP 2
	if (f2 () .ne. 45) STOP 3
	if (e2 () .ne. 45) STOP 4
	if (f3 () .ne. 47) STOP 5
	if (e3 () .ne. 47) STOP 6
	d = 17
	if (f4 (d) .ne. 32) STOP 7
	if (e4 (d) .ne. 59) STOP 8
	if (f5 () .ne. 45) STOP 9
	if (e5 () .ne. 45) STOP 10
	if (f6 () .ne. 47) STOP 11
	if (e6 () .ne. 47) STOP 12
	if (f7 () .ne. 163) STOP 13
	if (e7 () .ne. 163) STOP 14
	if (f8 () .ne. 115) STOP 15
	if (e8 () .ne. 115) STOP 16
	if (f9 () .ne. 119) STOP 17
	if (e9 () .ne. 119) STOP 18
	end
