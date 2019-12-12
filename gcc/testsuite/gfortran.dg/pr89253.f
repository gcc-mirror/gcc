! { dg-do compile }
! { dg-additional-options "-fsplit-loops -fno-tree-dominator-opts -std=legacy -w" }
      program jr
      integer :: w5, pg, zh
      w5 = 0
      write (w5)
      assign 0002 to w5
      do pg = 1, 3
         if (pg .eq. 1) then
            do zh = 1, pg
            end do
         else
            goto w5
 0001       zh = 0
 0002       zh = 0
            assign 0001 to w5
         endif
      end do
      end
