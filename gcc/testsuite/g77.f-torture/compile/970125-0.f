	integer*4 i4
	integer*8 i8
	integer*8 max4
	data max4/2147483647/
	i4 = %loc(i4)
	i8 = %loc(i8)
	print *, max4
	print *, i4, %loc(i4)
	print *, i8, %loc(i8)
	call foo(i4, %loc(i4), i8, %loc(i8))
	end
	subroutine foo(i4, i4a, i8, i8a)
	integer*8 i8
	print *, i4, i4a
	print *, i8, i8a
	end
