! { dg-do run }
! { dg-additional-options "-Wno-intrinsic-shadow" }
! PR fortran/103782 - ICE overloading an intrinsic like dble or real
! Contributed by Urban Jost

program runtest
  implicit none
  interface dble
     procedure to_double
  end interface dble
  interface real
     procedure floor ! not really FLOOR...
  end interface real
  if (any (dble ([10.0d0,20.0d0]) - [10.0d0,20.0d0] /= 0.d0)) stop 1
  if (any (real ([1.5,2.5])       - [1.5,2.5]       /= 0.0 )) stop 2
contains
  elemental function to_double (valuein) result(d_out)
    doubleprecision,intent(in) :: valuein
    doubleprecision            :: d_out
    d_out=valuein
  end function to_double
  elemental function floor (valuein) result(d_out) ! not really FLOOR...
    real, intent(in) :: valuein
    real             :: d_out
    d_out=valuein
  end function floor
end program runtest
