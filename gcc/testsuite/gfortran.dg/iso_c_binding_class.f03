! { dg-do compile }
!
! PR 47023: C_Sizeof: Rejects valid code
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

  use iso_c_binding
  type t
    integer(c_int) :: i
  end type t
contains
  subroutine test(a) bind(c)  ! { dg-error "is not C interoperable" }
    class(t) :: a
  end subroutine
end 
