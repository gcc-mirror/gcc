! { dg-do run }
! Test assumed shape arrays in procedures with multiple entry points.
! Arguments that aren't present in all entry points must be treated like
! optional arguments.
module entry_4
contains
subroutine foo(a)
  integer, dimension(:) :: a
  integer, dimension(:) :: b
  a = (/1, 2/)
  return
entry bar(b)
  b = (/3, 4/)
end subroutine
end module

program entry_4_prog
  use entry_4
  integer :: a(2)
  a = 0
  call foo(a)
  if (any (a .ne. (/1, 2/))) call abort
  call bar(a)
  if (any (a .ne. (/3, 4/))) call abort
end program

! { dg-final { cleanup-modules "entry_4" } }
