! { dg-options "-O -fgraphite-identity -ffast-math -fno-tree-scev-cprop" }
      dimension b(12,8)
      do i=1,norb
      end do
      b(i,j) = 0
      call rdrsym(b)
      end
