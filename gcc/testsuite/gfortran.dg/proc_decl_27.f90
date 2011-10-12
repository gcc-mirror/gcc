! { dg-do compile }
!
! PR 50659: [4.5/4.6/4.7 Regression] [F03] ICE on invalid with procedure interface
!
! Contributed by Andrew Benson <abenson@caltech.edu>

module m1
  integer :: arrSize
end module

module m2
contains
  function Proc (arg)
    use m1
    double precision, dimension(arrSize) :: proc
    double precision :: arg
  end function
end

  use m2
  implicit none
  procedure(Proc) :: Proc_Get
end

! { dg-final { cleanup-modules "m1 m2" } }
