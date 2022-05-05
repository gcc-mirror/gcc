! { dg-do compile }
! PR fortran/104211 - ICE in find_array_section
! Contributed by G.Steinmetz

program p
  type t
     real :: n
  end type
  type(t), parameter :: a(3) = [t(2)] ! { dg-error "Different shape" }
  type(t), parameter :: b(2) = a(2:3) ! { dg-error "Error in array constructor" }
end
