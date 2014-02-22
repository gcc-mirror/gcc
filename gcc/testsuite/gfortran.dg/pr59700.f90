! { dg-do run }
! PR59700 Test case by Steve Kargl
program foo

   implicit none

   character(len=80) msg
   integer, parameter :: fd = 10
   integer i1, i2, i3, i4
   real    x1, x2, x3, x4
   complex c1, c2
   logical a

   open(unit=fd, status='scratch')
   write(fd, '(A)') '1 2 3.4 q'

   rewind(fd)
   msg = 'ok'
   read(fd, *, err=10, iomsg=msg) i1, i2, i3, i4
10 if (msg /= 'Bad integer for item 3 in list input') call abort
   rewind(fd)
   msg = 'ok'
   read(fd, *, err=20, iomsg=msg) x1, x2, x3, x4
20 if (msg /= 'Bad real number in item 4 of list input') call abort
   rewind(fd)
   msg = 'ok'
   read(fd, *, err=30, iomsg=msg) i1, x2, x1, a
30 if (msg /= 'Bad logical value while reading item 4') call abort
   rewind(fd)
   read(fd, *, err=31, iomsg=msg) i1, x2, a, x1
31 if (msg /= 'Bad repeat count in item 3 of list input') call abort
   close(fd)
   open(unit=fd, status='scratch')
   write(fd, '(A)') '(1, 2) (3.4, q)'
   rewind(fd)
   msg = 'ok'
   read(fd, *, err=40, iomsg=msg) c1, c2
40 if (msg /= 'Bad floating point number for item 2') call abort
   close(fd)
end program foo
