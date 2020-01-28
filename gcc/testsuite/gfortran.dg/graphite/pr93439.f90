! { dg-additional-options "-O2 -floop-parallelize-all -floop-unroll-and-jam -ftree-parallelize-loops=2" }

module ai
  integer, parameter :: dp = 8
contains
  subroutine qu(ja, nq, en, p5)
    real(kind = dp) :: nq(ja), en(ja), p5(ja)
    call tl(ja, nq, en, p5)
  end subroutine qu

  subroutine tl(ja, nq, en, p5)
    real(kind = dp) :: nq(9), en(9 * ja), p5(3 * ja)
    do mc = 1, ja
       do mb = 1, 9
          do ma = 1, 3
             p5((mc - 1) * 3 + ma) = p5((mc - 1) * 3 + ma) - 1
          end do
       end do
    end do
  end subroutine tl
end module ai
