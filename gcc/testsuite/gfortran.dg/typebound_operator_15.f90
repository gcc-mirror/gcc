! { dg-do run }
!
! PR fortran/53255
!
! Contributed by Reinhold Bader.
!
! Before TYPE(ext)'s .tr. wrongly called the base type's trace
! instead of ext's trace_ext.
!
module mod_base
  implicit none
  private
  integer, public :: base_cnt = 0
  type, public :: base
     private
     real :: r(2,2) = reshape( (/ 1.0, 2.0, 3.0, 4.0 /), (/ 2, 2 /))
   contains
     procedure, private :: trace
     generic :: operator(.tr.) => trace
  end type base
contains
  complex function trace(this)
    class(base), intent(in) :: this
    base_cnt = base_cnt + 1
!    write(*,*) 'executing base'
    trace = this%r(1,1) + this%r(2,2)
  end function trace
end module mod_base

module mod_ext
  use mod_base
  implicit none
  private
  integer, public :: ext_cnt = 0
  public :: base, base_cnt
  type, public, extends(base) :: ext
     private
     real :: i(2,2) = reshape( (/ 1.0, 1.0, 1.0, 1.5 /), (/ 2, 2 /))
   contains
     procedure, private :: trace => trace_ext
  end type ext
contains
   complex function trace_ext(this)
    class(ext), intent(in) :: this

!   the following should be executed through invoking .tr. p below
!    write(*,*) 'executing override'
    ext_cnt = ext_cnt + 1
    trace_ext = .tr. this%base + (0.0, 1.0) * ( this%i(1,1) + this%i(2,2) )
  end function trace_ext

end module mod_ext
program test_override
  use mod_ext
  implicit none
  type(base) :: o
  type(ext) :: p
  real :: r

  ! Note: ext's ".tr." (trace_ext) calls also base's "trace"

!  write(*,*) .tr. o
!  write(*,*) .tr. p
  if (base_cnt /= 0 .or. ext_cnt /= 0) call abort ()
  r = .tr. o
  if (base_cnt /= 1 .or. ext_cnt /= 0) call abort ()
  r = .tr. p
  if (base_cnt /= 2 .or. ext_cnt /= 1) call abort ()

  if (abs(.tr. o - 5.0 ) < 1.0e-6  .and. abs( .tr. p - (5.0,2.5)) < 1.0e-6) &
  then
    if (base_cnt /= 4 .or. ext_cnt /= 2) call abort ()
!     write(*,*) 'OK'
  else
    call abort()
!     write(*,*) 'FAIL'
  end if
end program test_override
