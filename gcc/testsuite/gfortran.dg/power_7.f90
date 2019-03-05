! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR 85544 - this used to ICE.
program p
   integer, parameter :: na = -3, ne = 10
   integer :: i, a(na:ne), b(na:ne)
   integer :: v
   a = [(i, i=na, ne)]
   b = [2**a]
   if (any (b /= [0,0,0,1,2,4,8,16,32,64,128,256,512,1024])) stop 1
   b = [1**a]
   if (any (b /= 1)) stop 2
   b = [(-1)**a]
   if (any (b /= [-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1]) )stop 3
   b = [8**a]
   if (any (b /= [0,0,0,1,8,64,512,4096,32768,262144,2097152,16777216,&
        134217728,1073741824])) stop 4
   b = [4**a]
   if (any (b /= [0,0,0,1,4,16,64,256,1024,4096,16384,65536,262144,1048576])) stop 5
   
   v = 1
   do i=1,6
      v = v * 16
      if (v /= 16**i) stop 6 
   end do
 end program p
! { dg-final { scan-tree-dump-not "_gfortran_pow" "original" } }
