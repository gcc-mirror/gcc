! { dg-do run }
!
! PR 41022: [F03] procedure pointer components as actual arguments
!
! Contributed by Juergen Reuter <reuter@physik.uni-freiburg.de>

program foo

   type :: container_t
      procedure(proc), nopass, pointer :: proc => null ()
   end type container_t

   type(container_t), target :: obj1
   type(container_t) :: obj2

   obj1%proc => proc
   call transfer_proc_ptr (obj2, obj1)

   if (obj2%proc()/=7) STOP 1

contains

   subroutine transfer_proc_ptr (obj2, obj1)
     type(container_t), intent(out) :: obj2
     type(container_t), intent(in), target :: obj1
     call assign_proc_ptr (obj2%proc, obj1)
   end subroutine transfer_proc_ptr

   subroutine assign_proc_ptr (ptr, obj1)
     procedure(proc), pointer :: ptr
     type(container_t), intent(in), target :: obj1
     ptr => obj1%proc
   end subroutine assign_proc_ptr

   integer function proc ()
      proc = 7
   end function

end program foo

