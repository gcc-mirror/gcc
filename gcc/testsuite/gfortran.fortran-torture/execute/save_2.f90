! PR fortran/18518
      program main
	call foo
	call bar
	call foo
      end program main

      subroutine foo
	integer i,g,h
	data i/0/
	equivalence (g,h)
	save g
	if (i == 0) then
	   i = 1
	   h = 12345
	end if
	if (h .ne. 12345) call abort
      end subroutine foo

      subroutine bar
	integer a(10)
	a = 34
      end subroutine bar
