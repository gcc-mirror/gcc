! { dg-do run }
! Test for ITIME and IDATE intrinsics
      integer x(3)
      call itime(x)
      if (x(1) < 0 .or. x(1) > 23 .or.
     &    x(2) < 0 .or. x(2) > 59 .or.
     &    x(3) < 0 .or. x(3) > 61) call abort
      call idate(x)
      if (x(1) < 1 .or. x(1) > 31 .or.
     &    x(2) < 1 .or. x(2) > 12 .or.
     &    x(3) < 2001 .or. x(3) > 2100) call abort
      end
