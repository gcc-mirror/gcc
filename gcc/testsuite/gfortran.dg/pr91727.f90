! { dg-do compile }
! Code contributed by Gerhard Steinmetz.
program p
   type t
      class(*), allocatable :: a
   end type
   type(t) :: x
   allocate (x%a, source=[1]) ! { dg-error "have the same rank as" }
end
