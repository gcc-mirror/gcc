	function foo ()
	foo = 4
	foo = foo / 2
	return
	entry bar ()
	bar = 9
	bar = bar / 3
	end

	program entrytest
	if (foo () .ne. 2) STOP 1
	if (bar () .ne. 3) STOP 2
	end
