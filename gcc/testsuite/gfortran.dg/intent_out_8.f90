! { dg-do compile }
! { dg-options "-Wall" }
!
! PR 53655: [F03] "default initializer" warnings
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

type t
end type t

contains

  subroutine foo(x)             ! { dg-warning "defined but not used" }
    type(t), intent(out) :: x
  end subroutine

end 
