! This failed with a bogus:
!   'must be of TYPE(C_PTR)'
module m
  implicit none
contains
  subroutine q()
  end
  subroutine one(x)
    integer :: x
  end
  subroutine two(x)
    !$omp declare variant(one) match(construct={dispatch}) adjust_args(nothing: x)
    integer :: x

    !$omp dispatch
      call q
  end
end
