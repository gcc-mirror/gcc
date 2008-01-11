! { dg-do run }
! { dg-options "-O0" }
! PR fortran/34537
! simplify_transfer used to ICE on divide by zero for cases like this,
! where the mold expression is a non-constant character expression.
!
! Testcase contributed by Tobias Burnus <burnus@gcc.gnu.org >
!
  character, pointer :: ptr(:)
  character(8) :: a
  allocate(ptr(9))
  ptr = transfer('Sample#0'//achar(0),ptr) ! Causes ICE
  if (any (ptr .ne. ['S','a','m','p','l','e','#','0',achar(0)])) call abort
  call test(a)
  if (a .ne. 'Sample#2') call abort
contains
  subroutine test(a)
    character(len=*) :: a
    a = transfer('Sample#2',a)
  end subroutine test
end
