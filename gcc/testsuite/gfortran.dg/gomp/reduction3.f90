! { dg-do compile }

module mreduction3
  interface
    function ior (a, b)
      integer :: ior, a, b
    end function
  end interface
contains
  function iand (a, b)
    integer :: iand, a, b
    iand = a + b
  end function
end module mreduction3
subroutine f1
  integer :: i, ior
  ior = 6
  i = 6
!$omp parallel reduction (ior:i) ! { dg-error "is not INTRINSIC procedure name" }
!$omp end parallel
end subroutine f1
subroutine f2
  integer :: i
  interface
    function ior (a, b)
      integer :: ior, a, b
    end function
  end interface
  i = 6
!$omp parallel reduction (ior:i) ! { dg-error "is not INTRINSIC procedure name" }
  i = ior (i, 3)
!$omp end parallel
end subroutine f2
subroutine f3
  integer :: i
  intrinsic ior
  i = 6
!$omp parallel reduction (ior:i)
  i = ior (i, 3)
!$omp end parallel
end subroutine f3
subroutine f4
  integer :: i, ior
  i = 6
!$omp parallel reduction (ior:i)
  ior = 4			 ! { dg-error "is not a variable" }
!$omp end parallel
end subroutine f4
subroutine f5
  use mreduction3
  integer :: i
  i = 6
!$omp parallel reduction (ior:i) ! { dg-error "is not INTRINSIC procedure name" }
  i = ior (i, 7)
!$omp end parallel
end subroutine f5
subroutine f6
  use mreduction3
  integer :: i
  i = 6
!$omp parallel reduction (iand:i) ! { dg-error "is not INTRINSIC procedure name" }
  i = iand (i, 18)
!$omp end parallel
end subroutine f6
! { dg-final { cleanup-modules "mreduction3" } }
