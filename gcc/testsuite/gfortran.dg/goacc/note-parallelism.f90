! Test the output of "-fopt-info-optimized-omp".

! { dg-additional-options "-fopt-info-optimized-omp" }

! See also "../../c-c++-common/goacc/note-parallelism.c".

program test
  implicit none

  integer x, y, z

  !$acc parallel
  do x = 1, 10
  end do
  !$acc end parallel

  !$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  do x = 1, 10
  end do

  !$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelis" }
  do x = 1, 10
  end do

  !$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
  do x = 1, 10
  end do

  !$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
  do x = 1, 10
  end do

  !$acc parallel loop gang vector ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
  do x = 1, 10
  end do

  !$acc parallel loop gang worker ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
  do x = 1, 10
  end do

  !$acc parallel loop worker vector ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
  do x = 1, 10
  end do

  !$acc parallel loop gang worker vector ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
  do x = 1, 10
  end do

  !$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
  do x = 1, 10
     !$acc loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
     do y = 1, 10
        !$acc loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
        do z = 1, 10
        end do
     end do
  end do

  !$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
  do x = 1, 10
  end do

  !$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
  do x = 1, 10
     !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
     do y = 1, 10
     end do
  end do

  !$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
  do x = 1, 10
     !$acc loop ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
     do y = 1, 10
        !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
        do z = 1, 10
        end do
     end do
  end do

  !$acc parallel
  do x = 1, 10
     !$acc loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
     do y = 1, 10
        !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
        do z = 1, 10
        end do
     end do
  end do
  !$acc end parallel

  !$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  do x = 1, 10
     !$acc loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
     do y = 1, 10
        !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
        do z = 1, 10
        end do
     end do
  end do

  !$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
  do x = 1, 10
     !$acc loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
     do y = 1, 10
        !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
        do z = 1, 10
        end do
     end do
  end do

  !$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
  do x = 1, 10
     !$acc loop ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
     do y = 1, 10
        !$acc loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
        do z = 1, 10
        end do
     end do
  end do

  !$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
  do x = 1, 10
     !$acc loop ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
     do y = 1, 10
        !$acc loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
        do z = 1, 10
        end do
     end do
  end do

end program test
