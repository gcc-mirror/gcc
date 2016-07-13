C { dg-do compile }
C { dg-additional-options "-fvect-cost-model=dynamic -fno-ipa-icf" }
      subroutine test2(x,y)
      real x(4),y(4)
      beta=3.141593
      do i=1,4
        y(i)=y(i)+beta*x(i)
      end do
      end

      subroutine test3(x,y)
      real x(4),y(4)
      beta=3.141593
      y=y+beta*x
      end
C { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target { vect_hw_misalign } } } }
