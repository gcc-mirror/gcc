      subroutine foo(f1,f2,f3,f4,f5,f6,f7,f8,f9,f0,g1,g2,g3)
      implicit none
      integer f4,f3,f2,f1
      integer g4,g5,g6,g7,g8,g9
      integer i1,i2,i3,i4,i5

      real*8 g1(f4,f3,f2,f1),g2(f4,f4,f3,f2,f1),g3(f4,f3,f2,f1)
      real*8 f0(f4,f4,f3,f2,f1),f9(f4,f4,f3,f2,f1),f8(f4,f4,f3,f2,f1)
      real*8 f7(f4,f4,f3,f2,f1),f6(f4,f4,f3,f2,f1),f5(f4,f4,f3,f2,f1)

      do i3=1,f1
         g8=mod(i3+f1-2,f1)+1
         g9=mod(i3,f1)+1
         do i4=1,f2
            g6=mod(i4+f2-2,f2)+1
            g7=mod(i4,f2)+1
            do i5=1,f3
               g4=mod(i5+f3-2,f3)+1
               g5=mod(i5,f3)+1
               do i1=1,f4
                  g3(i1,i5,i4,i3)=0.0d0
                  do i2=1,f4
                     g3(i1,i5,i4,i3)=g3(i1,i5,i4,i3)+
     1                    g2(i1,i2,i5,i4,i3)*g1(i2,i5,i4,i3)+
     2                    f0(i1,i2,i5,i4,i3)*g1(i2,g5,i4,i3)+
     3                    f9(i1,i2,i5,i4,i3)*g1(i2,i5,g7,i3)+
     4                    f8(i1,i2,i5,i4,i3)*g1(i2,i5,i4,g9)+
     5                    f7(i1,i2,i5,i4,i3)*g1(i2,g4,i4,i3)+
     6                    f6(i1,i2,i5,i4,i3)*g1(i2,i5,g6,i3)+
     7                    f5(i1,i2,i5,i4,i3)*g1(i2,i5,i4,g8)
                  enddo
               enddo
            enddo
         enddo
      enddo
      return
      end

! This is the kernel extracted from bwaves: this cannot be interchanged
! as the number of iterations for f4 is not known.

! { dg-final { scan-tree-dump-times "will be interchanged" 0 "graphite" } }
