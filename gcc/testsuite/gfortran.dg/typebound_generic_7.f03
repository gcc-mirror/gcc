! { dg-do compile }
!
! PR 44434: [OOP] ICE in in gfc_add_component_ref
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module foo_mod
  type foo
  contains
    procedure :: doit
    generic :: do => doit
  end type
contains
  subroutine  doit(a) 
    class(foo) :: a
  end subroutine
end module

program testd15
contains
  subroutine dodo(x)
    use foo_mod
    class(foo) :: x
    call x%do()
  end subroutine
end 
