! { dg-do compile }
! Tests the fix for PR55984.
!
! Contributed by Sylwester Arabas  <slayoo@staszic.waw.pl>
!
module bcd_m
  type, abstract :: bcd_t
    contains
    procedure(bcd_fill_halos), deferred :: fill_halos
  end type
  abstract interface
    subroutine bcd_fill_halos(this)
      import :: bcd_t
      class(bcd_t ) :: this
    end subroutine
  end interface
end module

module solver_m
  use bcd_m
  type, abstract :: solver_t
    integer :: n, hlo
    class(bcd_t), pointer :: bcx, bcy
    contains
    procedure(solver_advop), deferred :: advop
  end type
  abstract interface
    subroutine solver_advop(this)
      import solver_t
      class(solver_t) :: this
    end subroutine
  end interface
  contains
end module

module solver_mpdata_m
  use solver_m
  type :: mpdata_t
    class(bcd_t), pointer :: bcx, bcy
    contains
    procedure :: advop => mpdata_advop
  end type
  contains
  subroutine mpdata_advop(this)
    class(mpdata_t) :: this
    associate ( bcx => this%bcx, bcy => this%bcy )
      call bcx%fill_halos()
    end associate
  end subroutine
end module

  use solver_mpdata_m
  class(mpdata_t), allocatable :: that
  call mpdata_advop (that)
end

