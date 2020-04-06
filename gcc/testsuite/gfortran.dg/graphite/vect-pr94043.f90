! { dg-do compile }
! { dg-additional-options "-O3 -ftree-parallelize-loops=2 -fno-tree-dce" }

! As PR94043, test it to be compiled successfully without ICE.

program yw
      integer :: hx(6, 6)
      integer :: ps = 1, e2 = 1

      do ps = 1, 6
        do e2 = 1, 6
            hx(e2, ps) = 0
            if (ps >= 5 .and. e2 >= 5) then
                hx(e2, ps) = hx(1, 1)
            end if
        end do
      end do
end program
