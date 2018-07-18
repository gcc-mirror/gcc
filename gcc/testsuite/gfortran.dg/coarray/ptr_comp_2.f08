! { dg-do run }

program ptr_comp 
   type t
      integer, pointer :: z(:)
   end type
   type(t), save :: obj[*]
   integer, allocatable, target :: i(:)[:]

   if (associated(obj%z)) error stop "'z' should not be associated yet."
   allocate (obj%z(5))
   call f(obj)
   if (associated(obj%z)) error stop "'z' should not be associated anymore."

   allocate(i(7)[*], SOURCE=42)
   obj%z => i
   if (.not. allocated(i)) error stop "'i' no longer allocated."
   i = 15
   if (any(obj%z(:) /= 15)) error stop "'obj%z' is deep copy and not pointer."

   nullify (obj%z)
   if (.not. allocated(i)) error stop "'i' should still be allocated."
   if (associated(obj%z)) error stop "'obj%z' should not be associated anymore."

   obj%z => i
   call f(obj)
   ! One can not say anything about i here. The memory should be deallocated, but
   ! the pointer in i is still set.
   if (associated(obj%z)) error stop "'obj%z' should not be associated anymore."
contains
   subroutine f(x)
      type(t) :: x[*]
      if ( associated(x%z) ) deallocate(x%z)
   end subroutine
end program

