! { dg-do run }
! { dg-options "-fbounds-check" }
! PR fortran/32036
program test
  type t
    integer, dimension (5) :: field
  end type t
  type (t), dimension (2) :: a
  integer :: calls

  type xyz_type
     integer :: x
  end type xyz_type
  type (xyz_type), dimension(3) :: xyz
  character(len=80) :: s

  xyz(1)%x = 11111
  xyz(2)%x = 0
  xyz(3)%x = 0

  write(s,*) xyz(bar())
  if (trim(adjustl(s)) /= "11111") call abort

  a(1)%field = 0
  a(2)%field = 0
  calls = 0
  if (sum(a(foo(calls))%field) /= 0) call abort
  if (calls .ne. 1) call abort

contains

  function foo (calls)
    integer :: calls, foo
    calls = calls + 1
    foo = 2
  end function foo 

  integer function bar ()
    integer, save :: i = 1
    bar = i
    i = i + 1
  end function

end program test
