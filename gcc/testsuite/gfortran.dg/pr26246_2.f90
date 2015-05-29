! PR fortran/26246
! { dg-options "-fdump-tree-original -fno-automatic" }
! { dg-do compile }

subroutine foo(string, n)
  implicit none
  integer :: n
  character(len=n + 6), intent(in) :: string
  if (string .eq. 'abc') call abort
end subroutine foo

! { dg-final { scan-tree-dump-times "static int" 0 "original" } }
