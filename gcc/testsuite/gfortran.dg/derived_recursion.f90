! { dg-do compile }
! { dg-options "-O0" }
! Tests patch for PR24158 - The module would compile, in spite
! of the recursion between the derived types. This would cause
! an ICE in the commented out main program. The standard demands
! that derived type components be already defined, to break
! recursive derived type definitions.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
module snafu
  type       ::   a
    integer    :: v
    type(b)    :: i ! { dg-error "not been previously defined" }
  end type a
  type       ::   b
    type(a)    :: i
  end type b
  type (a)   :: foo
end module snafu

!  use snafu
!  foo%v = 1
!  end

! { dg-final { cleanup-modules "snafu" } }
