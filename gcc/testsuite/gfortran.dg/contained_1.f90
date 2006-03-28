! PR15986
! Siblings may be used as actual arguments, in which case they look like
! variables during parsing.  Also checks that actual variables aren't replaced
! by siblings with the same name
! { dg-do run }
module contained_1_mod
integer i
contains
subroutine a
  integer :: c = 42
  call sub(b, c)
end subroutine a
subroutine b()
  i = i + 1
end subroutine b
subroutine c
end subroutine
end module

subroutine sub (proc, var)
  external proc1
  integer var

  if (var .ne. 42) call abort
  call proc
end subroutine

program contained_1
  use contained_1_mod
  i = 0
  call a
  if (i .ne. 1) call abort
end program

! { dg-final { cleanup-modules "contained_1_mod" } }
