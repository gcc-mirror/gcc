! { dg-do compile }
! PR fortran/108434 - ICE in class_allocatable
! Contributed by G.Steinmetz  <gscfq@t-online.de>

program p
  type t
     class(c), pointer :: a(2) ! { dg-error "must have a deferred shape" }
  end type t
  type s
     class(d), allocatable :: a(2) ! { dg-error "must have a deferred shape|not been declared" }
  end type
  type u
     type(e),  allocatable :: b(2) ! { dg-error "must have a deferred shape|not been declared" }
  end type
  class(t), allocatable :: x
  class(t), pointer     :: y
  class(s), allocatable :: x2
  class(s), pointer :: y2
end
