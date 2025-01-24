! Reduced from SPEC2017 527.cam4 zm_conv.F90

! { dg-do compile }
! { dg-options "-Ofast -std=legacy -march=rv64gcv_zvl256b_zba_zbb_zbs_zicond -ftree-vectorize -mabi=lp64d" }

module a
  contains
subroutine b(f)

   real d(4)
   integer e(4)
   integer f(4)
   real hmax(4)
   real g(4)

   integer h(4)
   integer l(4,5)
   do i = 1,c
      h(i) = 0
   end do
   do k = j  ,1
      do i = 1,c
         q = g(i) + hmax(i)
         if (k >= nint(d(i)) .and. k <= e(i) .and. q > 1.e4) then
            f(i) = k
         end if
      if (k < o ) then
            if (buoy<= 0.) then
               l(i,h) = k
            end if
         end if
      end do
   end do
   do n = 1,5
      do k = 1,m
         do i = 1,c
            if (k > l(i,n)) then
               p = r()
            end if
         end do
      end do
   end do
end
end

! { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 1 } }
! { dg-final { scan-assembler-times {fsrmi\s+[01234]} 1 } }
! { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 1 } }
