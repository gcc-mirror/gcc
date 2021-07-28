! OpenACC 'routine' directives inside a Fortran module.

! { dg-additional-options "-fopt-info-optimized-omp" }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

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

  subroutine s_1_nh
    implicit none
    !$acc routine nohost

    integer :: i

    !$acc loop ! { dg-bogus "assigned OpenACC .* loop parallelism" }
    do i = 1, 3
    end do
  end subroutine s_1_nh

  subroutine s_2
    implicit none
    !$acc routine (s_2) seq

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
    ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
    do i = 1, 3
    end do
  end subroutine s_2

  subroutine s_2_nh
    implicit none
    !$acc routine (s_2_nh) seq nohost

    integer :: i

    !$acc loop ! { dg-bogus "assigned OpenACC .* loop parallelism" }
    do i = 1, 3
    end do
  end subroutine s_2_nh

  subroutine v_1
    implicit none
    !$acc routine vector

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
    do i = 1, 3
    end do
  end subroutine v_1

  subroutine v_1_nh
    implicit none
    !$acc routine vector nohost

    integer :: i

    !$acc loop ! { dg-bogus "assigned OpenACC .* loop parallelism" }
    do i = 1, 3
    end do
  end subroutine v_1_nh

  subroutine w_1
    implicit none
    !$acc routine (w_1) worker

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
    do i = 1, 3
    end do
  end subroutine w_1

  subroutine w_1_nh
    implicit none
    !$acc routine (w_1_nh) worker nohost

    integer :: i

    !$acc loop ! { dg-bogus "assigned OpenACC .* loop parallelism" }
    do i = 1, 3
    end do
  end subroutine w_1_nh

  subroutine g_1
    implicit none
    !$acc routine gang
    ! { dg-bogus "\[Ww\]arning: region is worker partitioned but does not contain worker partitioned code" "TODO default 'gang' 'vector'" { xfail *-*-* } .-3 }

    integer :: i

    !$acc loop ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
    do i = 1, 3
    end do
  end subroutine g_1

  subroutine g_1_nh
    implicit none
    !$acc routine gang nohost

    integer :: i

    !$acc loop ! { dg-bogus "assigned OpenACC .* loop parallelism" }
    do i = 1, 3
    end do
  end subroutine g_1_nh

  subroutine pl_1
    implicit none

    integer :: i

    !$acc parallel loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
    ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
    do i = 1, 3
       call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
       call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
       call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
       call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
       call v_1 ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
       call v_1_nh ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
       call w_1 ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
       call w_1_nh ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
       call g_1 ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
       call g_1_nh ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
    end do
  end subroutine pl_1
end module routine_module_mod_1
