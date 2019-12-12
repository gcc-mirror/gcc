! OpenACC 'routine' directives inside a Fortran module.

! { dg-additional-options "-fopt-info-optimized-omp" }

module routine_module_mod_1
contains
  subroutine s_1
    implicit none
    !$acc routine

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
    ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
    do i = 1, 3
    end do
  end subroutine s_1

  subroutine s_2
    implicit none
    !$acc routine (s_2) seq

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
    ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
    do i = 1, 3
    end do
  end subroutine s_2

  subroutine v_1
    implicit none
    !$acc routine vector

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
    do i = 1, 3
    end do
  end subroutine v_1

  subroutine w_1
    implicit none
    !$acc routine (w_1) worker

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
    do i = 1, 3
    end do
  end subroutine w_1

  subroutine g_1
    implicit none
    !$acc routine gang

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
    do i = 1, 3
    end do
  end subroutine g_1

  subroutine pl_1
    implicit none

    integer :: i

    !$acc parallel loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
    ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
    do i = 1, 3
       call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
       call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
       call v_1 ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
       call w_1 ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
       call g_1 ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
    end do
  end subroutine pl_1
end module routine_module_mod_1
