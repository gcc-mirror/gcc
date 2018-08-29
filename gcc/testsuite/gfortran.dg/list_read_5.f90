! { dg-do run }
! PR25307 Check handling of end-of-file conditions for list directed reads.
! Prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program pr25307
   character(len=10) :: str
   character(len=10) :: a(5)
   a=""
   a(1)="123"
   a(3)="234"
   str = '123'
! Check internal unit
   i = 0
   j = 0
   read( str, *, end=10 ) i,j
   STOP 1
10 continue
   if (i.ne.123) STOP 2
   if (j.ne.0) STOP 3
! Check file unit
   i = 0
   open(10, status="scratch")
   write(10,'(a)') "123"
   rewind(10)
   read(10, *, end=20) i,j
   STOP 4
20 continue
   if (i.ne.123) STOP 5
   if (j.ne.0) STOP 6
! Check internal array unit
   i = 0
   j = 0
   k = 0
   read(a(1:5:2),*, end=30)i,j,k
   STOP 7
30 continue
   if (i.ne.123) STOP 8
   if (j.ne.234) STOP 9
   if (k.ne.0) STOP 10
end program pr25307
