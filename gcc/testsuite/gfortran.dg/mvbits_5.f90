! { dg-do run }

! PR fortran/38887
! This aborted at runtime for the runtime zero-sized array arguments.

! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>

program try_ya0013
  integer ida(9)
  call ya0013(ida,1,5,6)
end program

SUBROUTINE YA0013(IDA,nf1,nf5,nf6)
  INTEGER IDA(9)
  IDA = 1
  CALL MVBITS(IDA(NF5:NF1), 0, 1, IDA(NF6:NF1),2)
END SUBROUTINE
