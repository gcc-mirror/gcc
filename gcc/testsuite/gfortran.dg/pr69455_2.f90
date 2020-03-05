! { dg-do run }
program foo
   block
      use, intrinsic :: ISO_FORTRAN_ENV, only: wp => REAL32, ik => INT32
      if (ik /= 4) stop 1
      if (wp /= 4) stop 2
   end block
   block
      use, intrinsic :: ISO_FORTRAN_ENV, only: wp => REAL64, ik => INT64
      if (ik /= 8) stop 3
      if (wp /= 8) stop 4
   end block
end program foo
