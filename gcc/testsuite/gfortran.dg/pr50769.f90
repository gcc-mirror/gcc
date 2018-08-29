! { dg-do compile }
! { dg-options "-O2 -ftree-tail-merge -fno-delete-null-pointer-checks -fno-guess-branch-probability" }
!
! based on testsuite/gfortran.dg/alloc_comp_optional_1.f90,
! which was contributed by David Kinniburgh <davidkinniburgh@yahoo.co.uk>
!
program test_iso
  type ivs
     character(LEN=1), dimension(:), allocatable :: chars
  end type ivs
  type(ivs) :: v_str
  integer :: i
  call foo(v_str, i)
  if (v_str%chars(1) .ne. "a") STOP 1
  if (i .ne. 0) STOP 2
  call foo(flag = i)
  if (i .ne. 1) STOP 3
contains
  subroutine foo (arg, flag)
    type(ivs), optional, intent(out) :: arg
    integer :: flag
    if (present(arg)) then
      arg = ivs([(char(i+96), i = 1,10)])
      flag = 0
    else
      flag = 1
    end if
  end subroutine
end

