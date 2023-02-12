! { dg-do compile }
! PR fortran/108434 - ICE in class_allocatable
! Contributed by G.Steinmetz

program p
  type t
     class(c), pointer :: a(2) ! { dg-error "must have a deferred shape" }
  end type t
  class(t), allocatable :: x
  class(t), pointer     :: y
end
