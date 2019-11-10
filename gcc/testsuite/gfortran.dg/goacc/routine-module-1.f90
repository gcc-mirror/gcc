! Valid use of routines defined inside a Fortran module.

! { dg-compile-aux-modules "routine-module-mod-1.f90" }
! { dg-additional-options "-fopt-info-optimized-omp" }

program main
  use routine_module_mod_1
  implicit none

  integer :: i


  call pl_1

  call sl_1


  !$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  do i = 1, 10
     call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call g_1 ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
     call g_1_nh ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
     call w_1 ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
     call w_1_nh ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
     call v_1 ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
     call v_1_nh ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  end do
  !$acc end parallel loop

  !$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
  do i = 1, 10
     call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call w_1 ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
     call w_1_nh ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
     call v_1 ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
     call v_1_nh ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  end do
  !$acc end parallel loop

  !$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
  do i = 1, 10
     call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call v_1 ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
     call v_1_nh ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  end do
  !$acc end parallel loop

  !$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  do i = 1, 10
     call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  end do
  !$acc end parallel loop


  !$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  do i = 1, 10
     call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call g_1 ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
     call g_1_nh ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
     call w_1 ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
     call w_1_nh ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
     call v_1 ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
     call v_1_nh ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  end do
  !$acc end serial loop

  !$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
  do i = 1, 10
     call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call w_1 ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
     call w_1_nh ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
     call v_1 ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
     call v_1_nh ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  end do
  !$acc end serial loop

  !$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
  do i = 1, 10
     call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call v_1 ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
     call v_1_nh ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  end do
  !$acc end serial loop

  !$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  do i = 1, 10
     call s_1 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_1_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     call s_2_nh ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  end do
  !$acc end serial loop

end program main
