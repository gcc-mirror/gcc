! { dg-do run }
! PR 17792
! PR 21375
! Test that the STAT argument to DEALLOCATE works with POINTERS and 
! ALLOCATABLE arrays.
program deallocate_stat
   
   implicit none

   integer i
   real, pointer :: a1(:), a2(:,:), a3(:,:,:), a4(:,:,:,:), &
   &  a5(:,:,:,:,:), a6(:,:,:,:,:,:), a7(:,:,:,:,:,:,:)

   real, allocatable :: b1(:), b2(:,:), b3(:,:,:), b4(:,:,:,:), &
   &  b5(:,:,:,:,:), b6(:,:,:,:,:,:), b7(:,:,:,:,:,:,:)

   allocate(a1(2), a2(2,2), a3(2,2,2), a4(2,2,2,2), a5(2,2,2,2,2))
   allocate(a6(2,2,2,2,2,2), a7(2,2,2,2,2,2,2))

   a1 = 1. ; a2 = 2. ; a3 = 3. ; a4 = 4. ; a5 = 5. ; a6 = 6. ; a7 = 7.

   i = 13
   deallocate(a1, stat=i) ; if (i /= 0) STOP 1
   deallocate(a2, stat=i) ; if (i /= 0) STOP 2
   deallocate(a3, stat=i) ; if (i /= 0) STOP 3
   deallocate(a4, stat=i) ; if (i /= 0) STOP 4
   deallocate(a5, stat=i) ; if (i /= 0) STOP 5
   deallocate(a6, stat=i) ; if (i /= 0) STOP 6
   deallocate(a7, stat=i) ; if (i /= 0) STOP 7

   i = 14
   deallocate(a1, stat=i) ; if (i /= 1) STOP 8
   deallocate(a2, stat=i) ; if (i /= 1) STOP 9
   deallocate(a3, stat=i) ; if (i /= 1) STOP 10
   deallocate(a4, stat=i) ; if (i /= 1) STOP 11
   deallocate(a5, stat=i) ; if (i /= 1) STOP 12
   deallocate(a6, stat=i) ; if (i /= 1) STOP 13
   deallocate(a7, stat=i) ; if (i /= 1) STOP 14

   allocate(b1(2), b2(2,2), b3(2,2,2), b4(2,2,2,2), b5(2,2,2,2,2))
   allocate(b6(2,2,2,2,2,2), b7(2,2,2,2,2,2,2))

   b1 = 1. ; b2 = 2. ; b3 = 3. ; b4 = 4. ; b5 = 5. ; b6 = 6. ; b7 = 7.

   i = 13
   deallocate(b1, stat=i) ; if (i /= 0) STOP 15
   deallocate(b2, stat=i) ; if (i /= 0) STOP 16
   deallocate(b3, stat=i) ; if (i /= 0) STOP 17
   deallocate(b4, stat=i) ; if (i /= 0) STOP 18
   deallocate(b5, stat=i) ; if (i /= 0) STOP 19
   deallocate(b6, stat=i) ; if (i /= 0) STOP 20
   deallocate(b7, stat=i) ; if (i /= 0) STOP 21

   i = 14
   deallocate(b1, stat=i) ; if (i /= 1) STOP 22
   deallocate(b2, stat=i) ; if (i /= 1) STOP 23
   deallocate(b3, stat=i) ; if (i /= 1) STOP 24
   deallocate(b4, stat=i) ; if (i /= 1) STOP 25
   deallocate(b5, stat=i) ; if (i /= 1) STOP 26
   deallocate(b6, stat=i) ; if (i /= 1) STOP 27
   deallocate(b7, stat=i) ; if (i /= 1) STOP 28


   allocate(a1(2), a2(2,2), a3(2,2,2), b4(2,2,2,2), b5(2,2,2,2,2))
   allocate(b6(2,2,2,2,2,2))

   a1 = 1. ; a2 = 2. ; a3 = 3. ; b4 = 4. ; b5 = 5. ; b6 = 6.

   i = 13
   deallocate(a1, stat=i) ;         if (i /= 0) STOP 29
   deallocate(a2, a1, stat=i) ;     if (i /= 1) STOP 30
   deallocate(a1, a3, a2, stat=i) ; if (i /= 1) STOP 31
   deallocate(b4, stat=i) ;         if (i /= 0) STOP 32
   deallocate(b4, b5, stat=i) ;     if (i /= 1) STOP 33
   deallocate(b4, b5, b6, stat=i) ; if (i /= 1) STOP 34

end program deallocate_stat
