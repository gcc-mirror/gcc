! { dg-do run }
! PR fortran/85786
program test

   implicit none

   type :: p2d
      real, pointer :: p(:,:) => null()
   end type p2d
  
   type :: test_cs
      type(p2d), pointer :: v(:) => null()
   end type test_cs

   type(test_cs), pointer :: cs
   real, allocatable, target :: e(:,:)

   allocate(cs)
   if (associated(cs) .neqv. .true.) stop 1

   allocate(cs%v(2))
   if (associated(cs%v) .neqv. .true.) stop 2

   allocate(e(2,2))
   e = 42

   if (query_ptr(e, cs) .neqv. .true.) stop 3

   contains

      logical function query_ptr(f_ptr, cs)

         real, target, intent(in) :: f_ptr(:,:)
         type(test_cs), pointer, intent(inout) :: cs

         if (associated(cs)) then
            if (associated(cs%v) .neqv. .true.) stop 4
            cs%v(2)%p => f_ptr
            if (associated(cs%v(2)%p) .neqv. .true.) stop 5
            query_ptr = associated(cs%v(2)%p, f_ptr)
         else
            query_ptr = .false.
         end if
  end function query_ptr

end program test
