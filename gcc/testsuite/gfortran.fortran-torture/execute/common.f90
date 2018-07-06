! Program to test COMMON and EQUIVALENCE.
program common
   real (kind=8) a(8)
   real (kind=8) b(5), c(5)
   common /com1/b,c
   equivalence (a(1), b(2))
   b = 100
   c = 200
   call common_pass
   call common_par (a, b,c)
   call global_equiv
   call local_equiv
end

! Use common block to pass values
subroutine common_pass
   real (kind=8) a(8)
   real (kind=8) b(5), c(5)
   common /com1/b,c
   equivalence (a(1), b(2))
   if (any (a .ne. (/100,100,100,100,200,200,200,200/))) STOP 1
end subroutine

! Common variables as argument
subroutine common_par (a, b, c)
   real (kind=8) a(8), b(5), c(5)
   if (any (a .ne. (/100,100,100,100,200,200,200,200/))) STOP 2
   if (any (b .ne. (/100,100,100,100,100/))) STOP 3
   if (any (c .ne. (/200,200,200,200,200/))) STOP 4
end subroutine

! Global equivalence
subroutine global_equiv
   real (kind=8) a(8), b(5), c(5), x(8), y(4), z(4)
   common /com2/b, c, y, z
   equivalence (a(1), b(2))
   equivalence (x(4), y(1))
   b = 100
   c = 200
   y = 300
   z = 400
   if (any (a .ne. (/100,100,100,100,200,200,200,200/))) STOP 5
   if (any (x .ne. (/200,200,200,300,300,300,300,400/))) STOP 6
end

! Local equivalence
subroutine local_equiv
  real (kind=8) a(8), b(10)
  equivalence (a(1), b(3))
  b(1:5) = 100
  b(6:10) = 200
  if (any (a .ne. (/100,100,100,200,200,200,200,200/))) STOP 7
end subroutine
