! { dg-do compile { target aarch64-*-* } }
! { dg-additional-options "-w -Ofast -fdump-tree-ivopts-all" }

module a
integer, parameter :: b=3, c=b
contains
subroutine d(block)
integer f, col   , block(:, :, :), e
do f = 1, c
   do col = 1, c
             block(:f,                          :, e()) = do
     end do
  end do
  end
  end

! { dg-final { scan-tree-dump-not {Selected IV set for loop .+ niters, 3 IVs:} ivopts } }
! { dg-final { scan-tree-dump-times {Selected IV set for loop .+ niters, 2 IVs:} 1 ivopts } }
! { dg-final { scan-tree-dump-times {Selected IV set for loop .+ niters, 1 IVs:} 1 ivopts } }

