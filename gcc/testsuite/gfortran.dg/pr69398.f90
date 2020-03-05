! { dg-do compile }
! PR fortran/69398
! Contributed by Gerhard Steinmetz
program p
   type t
   end type
   class(t), allocatable :: z(:)
   target :: z(:)    ! { dg-error "Duplicate DIMENSION attribute" }
   allocate (z(2))
end

