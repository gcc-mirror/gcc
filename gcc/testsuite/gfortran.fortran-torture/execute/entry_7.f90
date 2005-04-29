! Test alternate entry points for functions when the result types
! of all entry points match

	function f1 (a)
	integer a, b
	integer, pointer :: f1, e1
	allocate (f1)
	f1 = 15 + a
	return
	entry e1 (b)
	allocate (e1)
	e1 = 42 + b
	end function
	function f2 ()
	real, pointer :: f2, e2
	entry e2 ()
	allocate (e2)
	e2 = 45
	end function
	function f3 ()
	double precision, pointer :: f3, e3
	entry e3 ()
	allocate (f3)
	f3 = 47
	end function
	function f4 (a) result (r)
	double precision a, b
	double precision, pointer :: r, s
	allocate (r)
	r = 15 + a
	return
	entry e4 (b) result (s)
	allocate (s)
	s = 42 + b
	end function
	function f5 () result (r)
	integer, pointer :: r, s
	entry e5 () result (s)
	allocate (r)
	r = 45
	end function
	function f6 () result (r)
	real, pointer :: r, s
	entry e6 () result (s)
	allocate (s)
	s = 47
	end function

	program entrytest
	interface
	function f1 (a)
	integer a
	integer, pointer :: f1
	end function
	function e1 (b)
	integer b
	integer, pointer :: e1
	end function
	function f2 ()
	real, pointer :: f2
	end function
	function e2 ()
	real, pointer :: e2
	end function
	function f3 ()
	double precision, pointer :: f3
	end function
	function e3 ()
	double precision, pointer :: e3
	end function
	function f4 (a)
	double precision a
	double precision, pointer :: f4
	end function
	function e4 (b)
	double precision b
	double precision, pointer :: e4
	end function
	function f5 ()
	integer, pointer :: f5
	end function
	function e5 ()
	integer, pointer :: e5
	end function
	function f6 ()
	real, pointer :: f6
	end function
	function e6 ()
	real, pointer :: e6
	end function
	end interface
	double precision d
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
	end
