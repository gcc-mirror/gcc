! { dg-options "-O -ftree-pre -fgraphite-identity -fno-tree-copy-prop" }

      subroutine foo (ldmx,ldmy,v)
      integer :: ldmx, ldmy, v, l, m
      dimension v(5,ldmx,ldmy)
      do m = 5, 1, -1
        do l = m+1, 5
          v(m,3,2) = v(1,3,2)
        end do
        v(m,3,2) = m
      end do
      end
