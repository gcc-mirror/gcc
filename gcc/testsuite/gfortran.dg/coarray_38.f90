! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! Valid code - but currently not implemented for -fcoarray=lib; single okay 
!
subroutine one
implicit none
type t
  integer, allocatable :: a
  integer :: b
end type t
type t2
  type(t), allocatable :: caf2[:]
end type t2
type(t), save :: caf[*],x
type(t2) :: y

x = caf[4]     ! { dg-error "Sorry, coindexed coarray at \\(1\\) with allocatable component is not yet supported" }
x%a = caf[4]%a ! { dg-error "Sorry, coindexed access to a pointer or allocatable component of the coindexed coarray at \\(1\\) is not yet supported" }
x%b = caf[4]%b ! OK
x = y%caf2[5]  ! { dg-error "Sorry, coindexed coarray at \\(1\\) with allocatable component is not yet supported" }
x%a = y%caf2[4]%a ! { dg-error "Sorry, coindexed access to a pointer or allocatable component of the coindexed coarray at \\(1\\) is not yet supported" }
x%b = y%caf2[4]%b ! OK
end subroutine one

subroutine two
implicit none
type t
  integer, pointer :: a
  integer :: b
end type t
type t2
  type(t), allocatable :: caf2[:]
end type t2
type(t), save :: caf[*],x
type(t2) :: y

x = caf[4]     ! OK
x%a = caf[4]%a ! { dg-error "Sorry, coindexed access to a pointer or allocatable component of the coindexed coarray at \\(1\\) is not yet supported" }
x%b = caf[4]%b ! OK
x = y%caf2[5]  ! OK
x%a = y%caf2[4]%a ! { dg-error "Sorry, coindexed access to a pointer or allocatable component of the coindexed coarray at \\(1\\) is not yet supported" }
x%b = y%caf2[4]%b ! OK
end subroutine two

subroutine three
implicit none
type t
  integer :: b
end type t
type t2
  type(t), allocatable :: caf2(:)[:]
end type t2
type(t), save :: caf(10)[*]
integer :: x(10)
type(t2) :: y

x(1) = caf(2)[4]%b ! OK
x(:) = caf(:)[4]%b ! { dg-error "Sorry, coindexed access at \\(1\\) to a scalar component with an array partref is not yet supported" }

x(1) = y%caf2(2)[4]%b ! OK
x(:) = y%caf2(:)[4]%b ! { dg-error "Sorry, coindexed access at \\(1\\) to a scalar component with an array partref is not yet supported" }
end subroutine three

subroutine four
implicit none
type t
  integer, allocatable :: a
  integer :: b
end type t
type t2
  class(t), allocatable :: caf2[:]
end type t2
class(t), allocatable :: caf[:]
type(t) :: x
type(t2) :: y

!x = caf[4]    ! Unsupported - and ICEs in resolve_ordinary_assign, cf. PR fortran/65397
x%a = caf[4]%a ! { dg-error "Sorry, coindexed access to a pointer or allocatable component of the coindexed coarray at \\(1\\) is not yet supported" }
x%b = caf[4]%b ! OK
!x = y%caf2[5] ! Unsupported - and ICEs in resolve_ordinary_assign, cf. PR fortran/65397
x%a = y%caf2[4]%a ! { dg-error "Sorry, coindexed access to a pointer or allocatable component of the coindexed coarray at \\(1\\) is not yet supported" }
x%b = y%caf2[4]%b ! OK
end subroutine four

subroutine five
implicit none
type t
  integer, pointer :: a
  integer :: b
end type t
type t2
  class(t), allocatable :: caf2[:]
end type t2
class(t), save, allocatable :: caf[:]
type(t) :: x
type(t2) :: y

!x = caf[4]     ! OK - but ICEs in resolve_ordinary_assign, cf. PR fortran/65397
x%a = caf[4]%a ! { dg-error "Sorry, coindexed access to a pointer or allocatable component of the coindexed coarray at \\(1\\) is not yet supported" }
x%b = caf[4]%b ! OK
!x = y%caf2[5]  ! OK - but ICEs in resolve_ordinary_assign, cf. PR fortran/65397
x%a = y%caf2[4]%a ! { dg-error "Sorry, coindexed access to a pointer or allocatable component of the coindexed coarray at \\(1\\) is not yet supported" }
x%b = y%caf2[4]%b ! OK
end subroutine five

subroutine six
implicit none
type t
  integer :: b
end type t
type t2
  class(t), allocatable :: caf2(:)[:]
end type t2
class(t), save, allocatable :: caf(:)[:]
integer :: x(10)
type(t2) :: y

x(1) = caf(2)[4]%b ! OK
x(:) = caf(:)[4]%b ! { dg-error "Sorry, coindexed access at \\(1\\) to a scalar component with an array partref is not yet supported" }

x(1) = y%caf2(2)[4]%b ! OK
x(:) = y%caf2(:)[4]%b ! { dg-error "Sorry, coindexed access at \\(1\\) to a scalar component with an array partref is not yet supported" }
end subroutine six
