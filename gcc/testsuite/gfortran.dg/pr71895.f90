! { dg-do compile }
program p
   type t
      integer :: n
   end type
   type(t) :: x
   class(t) :: y        ! { dg-error "must be dummy, allocatable or pointer" }
   print *, extends_type_of(x, y)
   print *, extends_type_of(y, x)
end
