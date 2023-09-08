! { dg-do compile }
!
! Tests the fix for PR109948
!
! Contributed by Rimvydas Jasinskas <rimvydas.jas@gmail.com>
!
module mm
  implicit none
  interface operator(==)
    module procedure eq_1_2
  end interface operator(==)
  private :: eq_1_2
contains
  logical function eq_1_2 (x, y)
    integer, intent(in) :: x(:)
    real,    intent(in) :: y(:,:)
    eq_1_2 = .true.
  end function eq_1_2
end module mm

program pr109948
  use mm
  implicit none
  type tlap
    integer,    allocatable :: z(:)
  end type tlap
  type ulap
    type(tlap) :: u(2)
  end type ulap
  integer :: pid = 1
  call comment0         ! Original problem
  call comment1
  call comment3 ([5,4,3,2,1])
  call comment10
  call comment11 ([5,4,3,2,1])
contains
  subroutine comment0
    type(tlap) :: y_in
    integer :: x_out(3) =[0.0,0.0,0.0]
    y_in%z = [1,-2,3]
    call foo(y_in, x_out)
    if (any (x_out .ne. [0, -2, 0])) stop 1
    call foo(y_in, x_out)
    if (any (x_out .ne. [1, -2, 3])) stop 2
  end subroutine comment0

  subroutine foo(y, x)
    type(tlap) :: y
    integer :: x(:)
    associate(z=>y%z)
      if (pid == 1) then
        where ( z < 0 ) x(:) = z(:)
      else
        where ( z > 0 ) x(:) = z(:)
    endif
    pid = pid + 1
    end associate
  end subroutine foo

  subroutine comment1
    type(tlap) :: grib
    integer :: i
    grib%z = [3,2,1]
    associate(k=>grib%z)
      i = k(1)
      if (any(k==1)) i = 1
    end associate
    if (i .eq. 3) stop 3
  end subroutine comment1

  subroutine comment3(k_2d)
    implicit none
    integer :: k_2d(:)
    integer :: i
    associate(k=>k_2d)
      i = k(1)
      if (any(k==1)) i = 1
    end associate
    if (i .eq. 3) stop 4
  end subroutine comment3

  subroutine comment11(k_2d)
    implicit none
    integer :: k_2d(:)
    integer :: m(1) = 42
    real    :: r(1,1) = 3.0
    if ((m == r) .neqv. .true.) stop 5
    associate (k=>k_2d)
      if ((k == r) .neqv. .true.) stop 6  ! failed to find user defined operator
    end associate
    associate (k=>k_2d(:))
      if ((k == r) .neqv. .true.) stop 7
    end associate
  end subroutine comment11

  subroutine comment10
    implicit none
    type(ulap) :: z(2)
    integer :: i
    real    :: r(1,1) = 3.0
    z(1)%u = [tlap([1,2,3]),tlap([4,5,6])]
    z(2)%u = [tlap([7,8,9]),tlap([10,11,12])]
    associate (k=>z(2)%u(1)%z)
      i = k(1)
      if (any(k==8)) i = 1
    end associate
    if (i .ne. 1) stop 8
    associate (k=>z(1)%u(2)%z)
      if ((k == r) .neqv. .true.) stop 9
      if (any (k .ne. [4,5,6])) stop 10
    end associate
  end subroutine comment10
end program pr109948

