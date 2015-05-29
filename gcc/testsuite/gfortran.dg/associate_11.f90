! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/55134
!
! Contributed by Valery Weber
!
program bug
  implicit none
  integer,dimension(1)::i
  i(:)=1
  associate(a =>i)
    call foo(a)
  end associate
! write(*,*) i
  if (i(1) /= 2) call abort
contains
  subroutine foo(v)
    integer, dimension(*) :: v
    v(1)=2
  end subroutine foo
end program bug

! { dg-final { scan-tree-dump-times "foo ..integer.kind=4..0:. . restrict. a.data.;" 1 "original" } }
