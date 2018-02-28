! Program to test the power (**) operator
program testpow
   implicit none
   real(kind=4) r, s, two
   real(kind=8) :: q
   complex(kind=4) :: c, z
   real, parameter :: del = 0.0001
   integer i, j

   i = 2
   j = i ** 10
   if (abs (j - 1024) .gt. del) STOP 1
   j = i ** (-10)
   if (abs (j - 0) .gt. del) STOP 2
   j = i ** 0
   if (abs (j - 1) .gt. del) STOP 3
   i = 1
   j = i ** 10
   if (abs (j - 1) .gt. del) STOP 4
   j = i ** (-10)
   if (abs (j - 1) .gt. del) STOP 5
   j = i ** 0
   if (abs (j - 1) .gt. del) STOP 6
   i = -1
   j = i ** 10
   if (abs (j - 1) .gt. del) STOP 7
   j = i ** (-10)
   if (abs (j - 1) .gt. del) STOP 8
   j = i ** 0
   if (abs (j - 1) .gt. del) STOP 9
   j = i ** 11
   if (abs (j - (-1)) .gt. del) STOP 10
   j = i ** (-11)
   if (abs (j - (-1)) .gt. del) STOP 11

   c = (2.0, 3.0)
   z = c ** 2
   if (abs(z - (-5.0, 12.0)) .gt. del) STOP 12
   z = c ** 7
   if (abs(z - (6554.0, 4449.0)) .gt. del) STOP 13

   two = 2.0

   r = two ** 1
   if (abs (r - 2.0) .gt. del) STOP 14
   r = two ** 2
   if (abs (r - 4.0) .gt. del) STOP 15
   r = two ** 3
   if (abs (r - 8.0) .gt. del) STOP 16
   r = two ** 4
   if (abs (r - 16.0) .gt. del) STOP 17
   r = two ** 0
   if (abs (r - 1.0) .gt. del) STOP 18
   r = two ** (-1)
   if (abs (r - 0.5) .gt. del) STOP 19
   r = two ** (-2)
   if (abs (r - 0.25) .gt. del) STOP 20
   r = two ** (-4)
   if (abs (r - 0.0625) .gt. del) STOP 21
   s = 3.0
   r = two ** s
   if (abs (r - 8.0) .gt. del) STOP 22
   s = -3.0
   r = two ** s
   if (abs (r - 0.125) .gt. del) STOP 23
   i = 3
   r = two ** i
   if (abs (r - 8.0) .gt. del) STOP 24
   i = -3
   r = two ** i
   if (abs (r - 0.125) .gt. del) STOP 25
   c = (2.0, 3.0)
   c = c ** two
   if (abs(c - (-5.0, 12.0)) .gt. del) STOP 26
end program
