! { dg-do compile }
!
! PR fortran/41600
!
  implicit none
  type t
     integer :: X = -999.0
  end type t
  class(t), allocatable :: y(:)
  allocate (t :: y(1))
end
