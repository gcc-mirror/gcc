! { dg-options "-O3" }
! { dg-additional-options "-mcpu=neoverse-v1" { target aarch64*-*-* } }

SUBROUTINE fn0(ma, mb, nt)
  CHARACTER ca
  REAL r0(ma)
  INTEGER i0(mb)
  REAL r1(3,mb)
  REAL r2(3,mb)
  REAL r3(3,3)
  zero=0.0
  do na = 1, nt
     nt = i0(na)
     do l = 1, 3
        r1 (l, na) =   r0 (nt)
        r2(l, na) = zero
     enddo
  enddo
  if (ca  .ne.'z') then
     do j = 1, 3
        do i = 1, 3
           r4  = zero
        enddo
     enddo
     do na = 1, nt
        do k =  1, 3
           do l = 1, 3
              do m = 1, 3
                 r3 = r4 * v
              enddo
           enddo
        enddo
     do i = 1, 3
           do k = 1, ifn (r3)
           enddo
        enddo
     enddo
     endif
END
