! { dg-do compile }
!
! PR 82257: ICE in gfc_typename(), compare_rank(), resolve_structure_cons()

module m1

implicit none

  type,abstract :: c_base
  contains
    procedure(i1),private,deferred :: f_base
  end type c_base

  abstract interface
    function i1(this) result(res)
      import
      class(c_base),intent(IN) :: this
      class(c_base), pointer :: res
    end function i1
  end interface

  type,abstract,extends(c_base) :: c_derived
  contains
    procedure :: f_base => f_derived ! { dg-error "Type mismatch in function result \\(CLASS\\(\\*\\)/CLASS\\(c_base\\)\\)" }
  end type c_derived

contains

  function f_derived(this) result(res) ! { dg-error "must be dummy, allocatable or pointer" }
    class(c_derived), intent(IN) :: this
    class(*) :: res
  end function f_derived

end module m1

module m2

implicit none

  type :: t
  contains
    procedure :: p
  end type t

contains

  class(*) function p(this) ! { dg-error "must be dummy, allocatable or pointer" }
    class(t), intent(IN) :: this
  end function p

end module m2
