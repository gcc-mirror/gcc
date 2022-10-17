! { dg-do run }
! PR fortran/99348
! PR fortran/102521
! Check simplifications for initialization of DT parameter arrays

program p
  type t
     integer :: n
  end type
  type(t), parameter :: a(4)   = t(1)
  type(t), parameter :: d(*)   = a
  type(t), parameter :: b(2,2) = reshape(d, [2,2])
  integer, parameter :: nn     = b(2,2)% n
  type u
     character(3) :: c
  end type
  type(u),      parameter :: x(2,3) = u('ab')
  type(u),      parameter :: y(*,*) = transpose (x)
  character(*), parameter :: c      = y(3,2)% c
  integer,      parameter :: lc     = c% len
  integer,      parameter :: lyc    = len (y(3,2)% c)
! integer,      parameter :: lxc    = x(1,1)% c% len    ! fails (pr101735?)
  if (nn /= 1) stop 1
  if (lc /= 3 .or. lyc /= 3 .or. c /= "ab ") stop 2
end
