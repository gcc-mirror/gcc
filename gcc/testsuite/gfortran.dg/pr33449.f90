! { dg-do compile }
! { dg-options "-O2 -ftree-vectorize" }
!
! Testcase for vectorization (see PR33449).
!
subroutine dlarre (w, iblock, work)
  integer m, i, iblock(*)
  double precision w(*), work(*)

  m = 0
  do jblk = 1, 10
    do i = 1, 10
      m = m + 1
      w(m) = -work(i)
      iblock(m) = 0
    end do
  end do
end
