! { dg-do compile }
real a(10), b(10), c(10)
a = 0.
b = 1.
call sum(a, b, c, 10)
print *, c(5)
end
subroutine sum(a, b, c, n)
integer i, n
real a(n), b(n), c(n)
do i = 1, n
   c(i) = a(i) + b(i)
enddo
end
