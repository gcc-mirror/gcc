! { dg-do run }
! { dg-options "-funsigned" }
program memain
  implicit none
  unsigned(1) i1,j1
  unsigned(2) i2,j2
  unsigned(4) i4,j4
  unsigned(8) i8,j8
  integer ibits,n

  ibits=bit_size(1u_1)
  do n=1,ibits
     i1=huge(i1)
     call mvbits(1u_1, 0,n,i1,0)
     j1=uint(-1-2_1**n+2)
     if(i1.ne.j1) error stop 1
  enddo
  ibits=bit_size(1u_2)
  do n=1,ibits
     i2=huge(i2)
     call mvbits(1u_2, 0,n,i2,0)
     j2=uint(-1-2_2**n+2)
     if(i2.ne.j2) error stop 2
  enddo
  ibits=bit_size(1u_4)
  do n=1,ibits
     i4=huge(i4)
     call mvbits(1u_4, 0,n,i4,0)
     j4=uint(-1-2_4**n+2)
     if(i4.ne.j4) error stop 3
  enddo
  ibits=bit_size(1_8)
  do n=1,ibits
     i8=huge(i8)
     call mvbits(1u_8, 0,n,i8,0)
     j8=uint(-1-2_8**n+2,8)
     if(i8.ne.j8) error stop 4
  enddo

end program memain
