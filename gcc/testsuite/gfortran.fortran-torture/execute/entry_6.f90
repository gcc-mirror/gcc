! Test alternate entry points for functions when the result types
! of all entry points match

	function f1 (a)
	integer, dimension (2, 2) :: a, b, f1, e1
	f1 (:, :) = 15 + a (1, 1)
	return
	entry e1 (b)
	e1 (:, :) = 42 + b (1, 1)
	end function
	function f2 ()
	real, dimension (2, 2) :: f2, e2
	entry e2 ()
	e2 (:, :) = 45
	end function
	function f3 ()
	double precision, dimension (2, 2) :: a, b, f3, e3
	entry e3 ()
	f3 (:, :) = 47
	end function
	function f4 (a) result (r)
	double precision, dimension (2, 2) :: a, b, r, s
	r (:, :) = 15 + a (1, 1)
	return
	entry e4 (b) result (s)
	s (:, :) = 42 + b (1, 1)
	end function
	function f5 () result (r)
	integer, dimension (2, 2) :: r, s
	entry e5 () result (s)
	r (:, :) = 45
	end function
	function f6 () result (r)
	real, dimension (2, 2) :: r, s
	entry e6 () result (s)
	s (:, :) = 47
	end function

	program entrytest
	interface
	function f1 (a)
	integer, dimension (2, 2) :: a, f1
	end function
	function e1 (b)
	integer, dimension (2, 2) :: b, e1
	end function
	function f2 ()
	real, dimension (2, 2) :: f2
	end function
	function e2 ()
	real, dimension (2, 2) :: e2
	end function
	function f3 ()
	double precision, dimension (2, 2) :: f3
	end function
	function e3 ()
	double precision, dimension (2, 2) :: e3
	end function
	function f4 (a)
 	double precision, dimension (2, 2) :: a, f4
	end function
	function e4 (b)
 	double precision, dimension (2, 2) :: b, e4
	end function
	function f5 ()
	integer, dimension (2, 2) :: f5
	end function
	function e5 ()
	integer, dimension (2, 2) :: e5
	end function
	function f6 ()
	real, dimension (2, 2) :: f6
	end function
	function e6 ()
	real, dimension (2, 2) :: e6
	end function
	end interface
	integer, dimension (2, 2) :: i, j
	real, dimension (2, 2) :: r
	double precision, dimension (2, 2) :: d, e
	i (:, :) = 6
	j = f1 (i)
	if (any (j .ne. 21)) call abort ()
	i (:, :) = 7
	j = e1 (i)
	j (:, :) = 49
	if (any (j .ne. 49)) call abort ()
	r = f2 ()
	if (any (r .ne. 45)) call abort ()
	r = e2 ()
	if (any (r .ne. 45)) call abort ()
	e = f3 ()
	if (any (e .ne. 47)) call abort ()
	e = e3 ()
	if (any (e .ne. 47)) call abort ()
	d (:, :) = 17
	e = f4 (d)
	if (any (e .ne. 32)) call abort ()
	e = e4 (d)
	if (any (e .ne. 59)) call abort ()
	j = f5 ()
	if (any (j .ne. 45)) call abort ()
	j = e5 ()
	if (any (j .ne. 45)) call abort ()
	r = f6 ()
	if (any (r .ne. 47)) call abort ()
	r = e6 ()
	if (any (r .ne. 47)) call abort ()
	end
