! Ensure that the auto clause falls back to seq parallelism when the
! OpenACC loop is not explicitly independent.

! { dg-additional-options "-fopt-info-optimized-omp" }

program test
  implicit none
  integer, parameter :: n = 100
  integer i, j, k, l
  
  !$acc parallel loop auto ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  do i = 1, n
     !$acc loop auto independent ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
     do j = 1, n
        !$acc loop worker vector ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
        do k = 1, n
        end do
     end do
  end do
 
  !$acc parallel loop auto independent ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
  do i = 1, n
     !$acc loop auto ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     do j = 1, n
        !$acc loop auto ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
        do k = 1, n
           !$acc loop auto independent ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
           do l = 1, n
           end do
        end do
     end do
  end do

  !$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
  do i = 1, n
     !$acc loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
     do j = 1, n
        !$acc loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
        do k = 1, n
           !$acc loop auto independent ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
           ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
           do l = 1, n
           end do
           !$acc loop auto ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
           do l = 1, n
           end do
        end do
     end do
  end do
  

  !$acc parallel loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
  do i = 1, n
     !$acc loop gang worker ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
     do j = 1, n
        !$acc loop auto ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
	do k = 1, n
          !$acc loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
          do l = 1, n
          end do
       end do
       !$acc loop auto independent ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
       do l = 1, n
       end do
    end do
    !$acc loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
    do j = 1, n
       !$acc loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
       do k = 1, n
       end do
    end do
  end do

  !$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
  do i = 1, n
     !$acc loop ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
     do j = 1, n
        !$acc loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
        do k = 1, n
           !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
           do l = 1, n
           end do
        end do
     end do
  end do
end program test
