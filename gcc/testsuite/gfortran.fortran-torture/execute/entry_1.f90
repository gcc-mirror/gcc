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
	if (f1 (6) .ne. 21) call abort ()
	if (e1 (7) .ne. 49) call abort ()
	if (f2 () .ne. 45) call abort ()
	if (e2 () .ne. 45) call abort ()
	if (f3 () .ne. 47) call abort ()
	if (e3 () .ne. 47) call abort ()
	d = 17
	if (f4 (d) .ne. 32) call abort ()
	if (e4 (d) .ne. 59) call abort ()
	if (f5 () .ne. 45) call abort ()
	if (e5 () .ne. 45) call abort ()
	if (f6 () .ne. 47) call abort ()
	if (e6 () .ne. 47) call abort ()
	if (f7 () .ne. 163) call abort ()
	if (e7 () .ne. 163) call abort ()
	if (f8 () .ne. 115) call abort ()
	if (e8 () .ne. 115) call abort ()
	if (f9 () .ne. 119) call abort ()
	if (e9 () .ne. 119) call abort ()
	end
