c============================================== test.f
                real x, y
		real*8 x1, y1
		x=0.
		y = erfc(x)
		if (y .ne. 1.) call abort

		x=1.1
		y = erfc(x)
		if (abs(y - .1197949) .ge. 1.e-6) call abort

* modified from x=10, y .gt. 1.5e-44 to avoid lack of -mieee on Alphas.
		x=8
		y = erfc(x)
		if (y .gt. 1.2e-28) call abort

		x1=0.
		y1 = erfc(x1)
		if (y1 .ne. 1.) call abort

		x1=1.1d0
		y1 = erfc(x1)
		if (abs(y1 - .1197949d0) .ge. 1.d-6) call abort

		x1=10
		y1 = erfc(x1)
		if (y1 .gt. 1.5d-44) call abort
		end
c=================================================
!output:
!  0.  1.875
!  1.10000002  1.48958981
!  10.  5.00220949E-06
!
!The values should be:
!erfc(0)=1
!erfc(1.1)= 0.1197949
!erfc(10)<1.543115467311259E-044
