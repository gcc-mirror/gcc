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
   call abort()
10 continue
   if (i.ne.123) call abort()
   if (j.ne.0) call abort()
! Check file unit
   i = 0
   write(10,'(a)') "123"
   rewind(10)
   read(10, *, end=20) i,j
   call abort()
20 continue
   if (i.ne.123) call abort()
   if (j.ne.0) call abort()
! Check internal array unit
   i = 0
   j = 0
   k = 0
   read(a(1:5:2),*, end=30)i,j,k
   call abort()
30 continue
   if (i.ne.123) call abort()
   if (j.ne.234) call abort()
   if (k.ne.0) call abort()
end program pr25307
