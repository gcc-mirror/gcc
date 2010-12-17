! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }

        subroutine test(nb,nx,r2)
        implicit none
        integer nb,nx,i,l
        real*8 r2(nb,nx)


             do i=1,nx
                do l=1,nb
                   r2(l,i)=0.0d0
                enddo
             enddo

        return
        end
! Verify that offset of the first element is simplified
! While we understand to combine x + ~x IVOPTs now messes things
! up by hiding that operation in casts to unsigned.
! { dg-final { scan-tree-dump-not "~" "optimized" { xfail *-*-* } } }
! { dg-final { cleanup-tree-dump "optimized" } }
