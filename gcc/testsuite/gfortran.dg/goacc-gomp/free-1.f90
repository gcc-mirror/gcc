! { dg-additional-options "-fdump-tree-original -Wunused-variable" }
implicit none
integer :: a,b,c,d,e,f,g,h

!$bogus

    !$bogus
!$& bogus
   !$& bogus

!$    a = 1
!$ b = 2
!$ c = &
!$3

!$ d = &
!$&4

  !$    e = 5
 !$ f = 6
   !$ g = &
 !$7

 !$ h = &
!$&8
      end

!{ dg-final { scan-tree-dump-times "a = 1;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "b = 2;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "c = 3;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "d = 4;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "e = 5;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "f = 6;" 1 "original" } }
!{ dg-final { scan-tree-dump-times "g = 7;" 1 "original" } }
