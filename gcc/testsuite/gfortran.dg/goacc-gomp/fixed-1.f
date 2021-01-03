! { dg-additional-options "-fdump-tree-original -Wunused-variable" }
      implicit none
      integer :: a,b,c,d,e,f,g,h,i,j,k,ll

c$bogus
!$bogus
*$bogus

c$ bogus
!$ bogus
*$ bogus

c$a23 bogus
!$ a  bogus
*$12a bogus

! The following should be parsed as OpenMP conditional sentinel
! If not, expect a unused-variable warning

c$    a = 1
!$    b = 2
*$    c = 3

c$ 1  d = 4
!$ 22 e = 5
*$34  f = 6

c$    g = 
c$   *7
!$ 2  h =
*$   & 8
*$ 3  i
!$   & = 9

c$    j
*$   &= 
c$   *10
!$ 5  k
*$   * =
c$   & 1
*$   & 1
*$9 9 ll
!$   & =
!$   *  12

c$ bogus
!$ bogus
*$ bogus

c$bogus
!$bogus
*$bogus

c$ acc bogus
!$ acc bogus
*$ acc bogus

c$ omp bogus
!$ omp bogus
*$ omp bogus
      end

!{ dg-final { scan-tree-dump-times "a = 1;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "b = 2;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "c = 3;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "d = 4;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "e = 5;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "f = 6;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "g = 7;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "h = 8;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "i = 9;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "j = 10;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "k = 11;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "ll = 12;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "__label_000001:;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "__label_000022:;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "__label_000034:;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "__label_000002:;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "__label_000003:;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "__label_000005:;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "__label_000099:;" 1 "original" } }
