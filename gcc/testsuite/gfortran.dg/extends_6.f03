! { dg-do compile }
! Some errors pointed out in the development of the patch.
!
! Contributed by Tobias Burnus  <burnus@net-b.de>
!
module m
  type :: date
    private
    integer :: yr, mon
    integer,public :: day
  end type
  type :: dt
    integer :: yr, mon
    integer :: day
  end type
end module m

  use m
  type, extends(date) :: datetime
    integer :: hr, min, sec
  end type
  type(datetime) :: o_dt

  type :: one
    integer :: i
  end type one

  type, extends(one) :: two
    real :: r
  end type two

  o_dt%day = 5  ! VALID but failed in first version of EXTENDS patch
  o_dt%yr  = 5  ! { dg-error "All components of 'date' are PRIVATE" }

  t = two(one = one(4), i = 5, r=4.4) ! { dg-error "has already been set" }

  call foo
contains
  subroutine foo
    use m, date_type => dt
    type, extends(date_type) :: dt_type
    end type
    type (dt_type) :: foo_dt
    foo_dt%date_type%day = 1
    foo_dt%dt%day = 1 ! { dg-error "not a member" }
  end subroutine
end

! { dg-final { cleanup-modules "m" } }
