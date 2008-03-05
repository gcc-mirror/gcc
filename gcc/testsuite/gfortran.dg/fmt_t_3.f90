! { dg-do run { target fd_truncate } }
! PR31051 bug with x and t format descriptors.
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org> from PR.
program t
   integer, parameter :: n = 9
   character(len=40) :: fmt
   character(len=2), dimension(n) :: y
   open(unit=10, status="scratch")
   y = 'a '
   fmt = '(a,1x,(t7, 3a))'
   write(10, fmt) 'xxxx', (y(i), i = 1,n)
   rewind(10)
   read(10, '(a)') fmt
   if (fmt.ne."xxxx  a a a") call abort()
end program t
