! { dg-do compile }
! Tests the fix for PR32881, in which the initialization
! of 'p' generated an error because the pureness of 'bar'
! escaped.
!
! Contributed by Janne Blomqvist <jb@gcc.gnu.org>
!
subroutine foo ()
  integer, pointer :: p => NULL()
contains
  pure function bar (a)
    integer, intent(in) :: a
    integer :: bar
    bar = a
  end function bar
end subroutine foo

