! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }

subroutine test(i)
type t
  real, allocatable :: x[:]
end type t

interface
  subroutine sub(y)
    import
    real :: y[*]
  end subroutine sub
end interface

integer :: i
type(t), save :: var
allocate(var%x[*])
call sub(var%x)
end subroutine test

! { dg-final { scan-tree-dump-times "sub \\(\\(real\\(kind=4\\) \\*\\) var.x.data, var.x.token, 0\\);" 1 "original" } }
