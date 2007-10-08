! { dg-do compile }
! PR 33689
! Wrongly rejected valid code due to non-trivial expression for array bound
 subroutine grylmr()
    integer, parameter :: lmaxd = 20
    REAL, save :: c(0:(lmaxd+1)*(lmaxd+1))
  end subroutine grylmr
end
