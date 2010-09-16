! { dg-do run }
!
! PR 45674: [OOP] Undefined references for extended types
!
! Contributed by Dietmar Ebner <dietmar.ebner@gmail.com>

module fails_mod
  implicit none 
  type :: a_t
     integer :: a
  end type
  type, extends(a_t) :: b_t
     integer :: b
  end type
contains
  subroutine foo(a)
    class(a_t) :: a
  end subroutine foo
end module fails_mod

module fails_test
  implicit none
contains
  subroutine bar
    use fails_mod
    type(b_t) :: b
    call foo(b)
  end subroutine bar
end module fails_test

end

! { dg-final { cleanup-modules "fails_mod fails_test" } }
