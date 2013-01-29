! { dg-do run }
! PR 25577
! MVBITS didn't work correctly for integer types wider than a C int
! The testcase is based on the one Dale Ranta posted in the bug report
implicit none
integer(1) i1,j1
integer(2) i2,j2
integer(4) i4,j4
integer(8) i8,j8
integer ibits,n

ibits=bit_size(1_1)
do n=1,ibits
   i1=-1
   call mvbits(1_1, 0,n,i1,0)
   j1=-1-2_1**n+2
   if(i1.ne.j1)call abort
enddo
ibits=bit_size(1_2)
do n=1,ibits
   i2=-1
   call mvbits(1_2, 0,n,i2,0)
   j2=-1-2_2**n+2
   if(i2.ne.j2)call abort
enddo
ibits=bit_size(1_4)
do n=1,ibits
   i4=-1
   call mvbits(1_4, 0,n,i4,0)
   j4=-1-2_4**n+2
   if(i4.ne.j4)call abort
enddo
ibits=bit_size(1_8)
do n=1,ibits
   i8=-1
   call mvbits(1_8, 0,n,i8,0)
   j8=-1-2_8**n+2
   if(i8.ne.j8)call abort
enddo
end
