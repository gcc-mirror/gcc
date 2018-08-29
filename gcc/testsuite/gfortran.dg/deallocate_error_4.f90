! { dg-do compile }
! PR fortran/82994
! Code contributed by Gerhard Steinmetz
program p
   type t
   end type
   class(t) :: x  ! { dg-error "must be dummy, allocatable or pointer" }
   allocate (x)   ! { dg-error "neither a data pointer nor an allocatable" }
   deallocate (x) ! { dg-error "not a nonprocedure pointer nor an allocatable" }
end
