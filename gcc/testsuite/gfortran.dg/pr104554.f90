! { dg-do compile }
! PR fortran/104554 - ICE in check_assumed_size_reference
! Contributed by G.Steinmetz

program p
  type t
     integer :: a
  end type
  class(t) :: x(*) ! { dg-error "Assumed size array" }
  x%a = 3
end
