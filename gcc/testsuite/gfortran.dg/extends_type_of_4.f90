! { dg-do compile }
! PR fortran/106121 - ICE in gfc_simplify_extends_type_of
! Contributed by G.Steinmetz

program p
   type t
   end type
   type(t)  :: x
   class(t) :: y               ! { dg-error "dummy, allocatable or pointer" }
   print *, extends_type_of (x, y)
end

subroutine s
   type t
      integer :: i
   end type
   type(t)  :: x
   class(t) :: y               ! { dg-error "dummy, allocatable or pointer" }
   stop extends_type_of (x, y) ! { dg-error "STOP code" }
end
