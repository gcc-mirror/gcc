! Program to test mathematical intrinsics
subroutine dotest (n, val4, val8, known)
   implicit none
   real(kind=4) val4, known
   real(kind=8) val8
   integer n

   if (abs (val4 - known) .gt. 0.001) call abort
   if (abs (real (val8, kind=4) - known) .gt. 0.001) call abort
end subroutine

subroutine dotestc (n, val4, val8, known)
   implicit none
   complex(kind=4) val4, known
   complex(kind=8) val8
   integer n
   if (abs (val4 - known) .gt. 0.001) call abort
   if (abs (cmplx (val8, kind=4) - known) .gt. 0.001) call abort
end subroutine

program testmath
   implicit none
   real(kind=4) r, two4, half4
   real(kind=8) q, two8, half8
   complex(kind=4) cr
   complex(kind=8) cq
   external dotest, dotest2

   two4 = 2.0
   two8 = 2.0_8
   half4 = 0.5
   half8 = 0.5_8
   r = sin (two4)
   q = sin (two8)
   call dotest (1, r, q, 0.9093)
   r = cos (two4)
   q = cos (two8)
   call dotest (2, r, q, -0.4161)
   r = tan (two4)
   q = tan (two8)
   call dotest (3, r, q, -2.1850)
   r = asin (half4)
   q = asin (half8)
   call dotest (4, r, q, 0.5234)
   r = acos (half4)
   q = acos (half8)
   call dotest (5, r, q, 1.0472)
   r = atan (half4)
   q = atan (half8)
   call dotest (6, r, q, 0.4636)
   r = atan2 (two4, half4)
   q = atan2 (two8, half8)
   call dotest (7, r, q, 1.3258)
   r = exp (two4)
   q = exp (two8)
   call dotest (8, r, q, 7.3891)
   r = log (two4)
   q = log (two8)
   call dotest (9, r, q, 0.6931)
   r = log10 (two4)
   q = log10 (two8)
   call dotest (10, r, q, 0.3010)
   r = sinh (two4)
   q = sinh (two8)
   call dotest (11, r, q, 3.6269)
   r = cosh (two4)
   q = cosh (two8)
   call dotest (12, r, q, 3.7622)
   r = tanh (two4)
   q = tanh (two8)
   call dotest (13, r, q, 0.9640)
   r = sqrt (two4)
   q = sqrt (two8)
   call dotest (14, r, q, 1.4142)

   r = atan2 (0.0, 1.0)
   q = atan2 (0.0_8, 1.0_8)
   call dotest (15, r, q, 0.0)
   r = atan2 (-1.0, 1.0)
   q = atan2 (-1.0_8, 1.0_8)
   call dotest (16, r, q, -0.7854)
   r = atan2 (0.0, -1.0)
   q = atan2 (0.0_8, -1.0_8)
   call dotest (17, r, q, 3.1416)
   r = atan2 (-1.0, -1.0)
   q = atan2 (-1.0_8, -1.0_8)
   call dotest (18, r, q, -2.3562)
   r = atan2 (1.0, 0.0)
   q = atan2 (1.0_8, 0.0_8)
   call dotest (19, r, q, 1.5708)
   r = atan2 (-1.0, 0.0)
   q = atan2 (-1.0_8, 0.0_8)
   call dotest (20, r, q, -1.5708)

   cr = log ((-1.0, -1.0))
   cq = log ((-1.0_8, -1.0_8))
   call dotestc (21, cr, cq, (0.3466, -2.3562))

end program

