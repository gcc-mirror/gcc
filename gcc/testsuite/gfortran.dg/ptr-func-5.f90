! { dg-do compile }
! PR fortran/109846
! CLASS pointer function result in variable definition context

module foo
  implicit none
  type :: parameter_list
  contains
    procedure :: sublist, sublist_nores
  end type
contains
  function sublist (this) result (slist)
    class(parameter_list), intent(inout) :: this
    class(parameter_list), pointer       :: slist
    allocate (slist)
  end function
  function sublist_nores (this)
    class(parameter_list), intent(inout) :: this
    class(parameter_list), pointer       :: sublist_nores
    allocate (sublist_nores)
  end function
end module

program example
  use foo
  implicit none
  type(parameter_list) :: plist
  call sub1 (plist%sublist())
  call sub1 (plist%sublist_nores())
  call sub2 (plist%sublist())
  call sub2 (plist%sublist_nores())
contains
  subroutine sub1 (plist)
    type(parameter_list), intent(inout) :: plist
  end subroutine
  subroutine sub2 (plist)
    type(parameter_list) :: plist
  end subroutine
end program
