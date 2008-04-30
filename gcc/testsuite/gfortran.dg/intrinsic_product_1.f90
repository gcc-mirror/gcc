! { dg-do run }
! PR 35993 - some intrinsics with mask = .false. didn't set
! the whole return array for multi-dimensional arrays.
! Test case adapted from Dick Hendrickson.

      program try

      call       ga3019(  1,  2,  3,  4)
      end program

      SUBROUTINE GA3019(nf1,nf2,nf3,nf4)
      INTEGER IDA(NF2,NF3)
      INTEGER IDA1(NF2,NF4,NF3)

      ida1 = 3

      ida = -3
      IDA(NF1:NF2,NF1:NF3) = PRODUCT(IDA1,NF2, NF1 .LT. 0)  !fails
      if (any(ida /= 1)) call abort

      ida = -3
      IDA(NF1:NF2,NF1:NF3) = PRODUCT(IDA1,NF2, .false. )    !fails
      if (any(ida /= 1)) call abort

      ida = -3
      IDA(NF1:NF2,NF1:NF3) = PRODUCT(IDA1,NF2, ida1 .eq. 137 )    !works
      if (any(ida /= 1)) call abort

      END SUBROUTINE
