! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/41587
!

type t0
  integer :: j = 42
end type t0

type t
  integer :: i
  class(t0), allocatable :: foo(3) ! { dg-error "must have a deferred shape" }
end type t

type t2
  integer :: i
  class(t0), pointer :: foo(3) ! { dg-error "must have a deferred shape" }
end type t2

type t3
  integer :: i
  class(t0), allocatable :: foo[3] ! { dg-error "Upper bound of last coarray dimension must be '\\*'" }
end type t3

end
