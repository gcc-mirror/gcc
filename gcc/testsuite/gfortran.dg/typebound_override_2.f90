! { dg-do compile }
!
! PR 47978: [OOP] Invalid INTENT in overriding TBP not detected
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module foo_mod
  type foo
  contains
    procedure, pass(f) :: bar => base_bar
  end type foo
contains 
  subroutine base_bar(f,j)
    class(foo), intent(inout) :: f
    integer, intent(in)    :: j
  end subroutine base_bar
end module foo_mod

module extfoo_mod
  use foo_mod
  type, extends(foo) :: extfoo
  contains
    procedure, pass(f) :: bar => ext_bar  ! { dg-error "INTENT mismatch in argument" }
  end type extfoo
contains 
  subroutine ext_bar(f,j)
    class(extfoo), intent(inout) :: f
    integer, intent(inout) :: j
  end subroutine ext_bar
end module extfoo_mod 
