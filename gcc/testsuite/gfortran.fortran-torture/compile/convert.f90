! Program to test conversion.  Does not actualy test the generated code
program convert
   implicit none
   integer(kind=4) i
   integer(kind=8) m
   real(kind=4) r
   real(kind=8) q
   complex(kind=4) c
   complex(kind=8) z

   ! each of these should generate a single intrinsic conversion expression
   i = int(i)
   i = int(m)
   i = int(r)
   i = int(q)
   i = int(c)
   i = int(z)
   m = int(i, kind=8)
   m = int(m, kind=8)
   m = int(r, kind=8)
   m = int(q, kind=8)
   m = int(c, kind=8)
   m = int(z, kind=8)
   r = real(i)
   r = real(m)
   r = real(r)
   r = real(q)
   r = real(c)
   r = real(z, kind=4)
   q = real(i, kind=8)
   q = real(m, kind=8)
   q = real(r, kind=8)
   q = real(q, kind=8)
   q = real(c, kind=8)
   ! Note real(<complex>) returns the type kind of the argument.
   q = real(z)
end program
