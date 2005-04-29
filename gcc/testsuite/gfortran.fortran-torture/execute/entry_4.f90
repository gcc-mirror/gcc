! Test alternate entry points for functions when the result types
! of all entry points don't match

	integer function f1 (a)
	integer a, b
	double precision e1
	f1 = 15 + a
	return
	entry e1 (b)
	e1 = 42 + b
	end function
	complex function f2 (a)
	integer a
	logical e2
	entry e2 (a)
	if (a .gt. 0) then
	  e2 = a .lt. 46
	else
	  f2 = 45
	endif
	end function
	function f3 (a) result (r)
	integer a, b
	real r
	logical s
	complex c
	r = 15 + a
	return
	entry e3 (b) result (s)
	s = b .eq. 42
	return
	entry g3 (b) result (c)
	c = b + 11
	end function
	function f4 (a) result (r)
	logical r
	integer a, s
	double precision t
	entry e4 (a) result (s)
	entry g4 (a) result (t)
	r = a .lt. 0
	if (a .eq. 0) s = 16 + a
	if (a .gt. 0) t = 17 + a
	end function

	program entrytest
	integer f1, e4
	real f3
	double precision e1, g4
	logical e2, e3, f4
	complex f2, g3
	if (f1 (6) .ne. 21) call abort ()
	if (e1 (7) .ne. 49) call abort ()
	if (f2 (0) .ne. 45) call abort ()
	if (.not. e2 (45)) call abort ()
	if (e2 (46)) call abort ()
	if (f3 (17) .ne. 32) call abort ()
	if (.not. e3 (42)) call abort ()
	if (e3 (41)) call abort ()
	if (g3 (12) .ne. 23) call abort ()
	if (.not. f4 (-5)) call abort ()
	if (e4 (0) .ne. 16) call abort ()
	if (g4 (2) .ne. 19) call abort ()
	end
