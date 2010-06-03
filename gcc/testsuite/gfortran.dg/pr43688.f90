! { dg-do compile }
! { dg-options "-O0 -fipa-reference" }

  subroutine sub
    type :: a
      integer :: i = 42
    end type a
    type(a), target :: dt(2)
    integer, pointer :: ip(:)
    ip => dt%i
  end subroutine
