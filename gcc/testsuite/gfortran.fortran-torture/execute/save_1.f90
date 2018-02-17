      subroutine foo (b)
	logical b
	integer i, j
	character*24 s
	save
	if (b) then
	  i = 26
	  j = 131
	  s = 'This is a test string'
	else
	  if (i .ne. 26 .or. j .ne. 131) STOP 1
	  if (s .ne. 'This is a test string') STOP 2
	end if
      end subroutine foo
      subroutine bar (s)
	character*42 s
	if (s .ne. '0123456789012345678901234567890123456') STOP 3
	call foo (.false.)
      end subroutine bar
      subroutine baz
	character*42 s
	! Just clobber stack a little bit.
	s = '0123456789012345678901234567890123456'
	call bar (s)
      end subroutine baz
      call foo (.true.)
      call baz
      call foo (.false.)
      end
