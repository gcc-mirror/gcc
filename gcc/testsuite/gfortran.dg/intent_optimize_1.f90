! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }
!
! Check whether the "does_not_exist" subroutine has been
! optimized away, i.e. check that "foo"'s intent(IN) gets
! honoured.
!
! PR fortran/43665


subroutine test

interface
  subroutine foo(x)
    integer, intent(in) :: x
  end subroutine foo
end interface

integer :: y

y = 5
call foo(y)
if (y /= 5) call does_not_exist ()
end

! { dg-final { scan-tree-dump-times "does_not_exist" 0 "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
