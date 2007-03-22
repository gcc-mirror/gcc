! { dg-do compile }
! PR 20897
! Make sure intrinsic type names do not appear as names of derived types
      type integer     ! { dg-error "cannot be the same as an intrinsic type" }
      type real        ! { dg-error "cannot be the same as an intrinsic type" }
      type complex     ! { dg-error "cannot be the same as an intrinsic type" }
      type character   ! { dg-error "cannot be the same as an intrinsic type" }
      type logical     ! { dg-error "cannot be the same as an intrinsic type" }
      type complex     ! { dg-error "cannot be the same as an intrinsic type" }
      type double precision     ! { dg-error "cannot be the same as an intrinsic type" }
      type doubleprecision      ! { dg-error "cannot be the same as an intrinsic type" }
      type double complex       ! { dg-error "cannot be the same as an intrinsic type" }
      type doublecomplex        ! { dg-error "cannot be the same as an intrinsic type" }

      type x
      integer y
      end type x
      end

