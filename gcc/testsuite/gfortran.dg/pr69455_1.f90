! { dg-do run }
program foo
   block
      use, intrinsic :: iso_c_binding, only: wp => c_float, ik => c_int
      if (ik /= 4) stop 1
      if (wp /= 4) stop 2
   end block
   block
      use, intrinsic :: iso_c_binding, only: wp => c_double, ik => c_int64_t
      if (ik /= 8) stop 3
      if (wp /= 8) stop 4
   end block
end program foo

