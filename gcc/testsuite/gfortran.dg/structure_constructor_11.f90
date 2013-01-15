! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/54603
!
! Contributed by Kacper Kowalik
!
module foo
   implicit none

   interface
      subroutine cg_ext
         implicit none
      end subroutine cg_ext
   end interface

   type :: ext_ptr
      procedure(cg_ext), nopass, pointer :: init
      procedure(cg_ext), nopass, pointer :: cleanup
   end type ext_ptr

   type :: ext_ptr_array
      type(ext_ptr) :: a
      contains
         procedure :: epa_init
   end type ext_ptr_array

   type(ext_ptr_array) :: bar

contains
   subroutine epa_init(this, init, cleanup)
      implicit none
      class(ext_ptr_array), intent(inout) :: this
      procedure(cg_ext), pointer, intent(in)    :: init
      procedure(cg_ext), pointer, intent(in)    :: cleanup

      this%a = ext_ptr(null(), null())  ! Wrong code
      this%a = ext_ptr(init, cleanup)  ! Wrong code

      this%a%init => init              ! OK
      this%a%cleanup => cleanup        ! OK

      this%a = ext_ptr(this%a%init,this%a%cleanup) ! ICE in fold_convert_loc
   end subroutine epa_init

end module foo

program ala
   use foo, only: bar
   implicit none
   integer :: count1, count2
   count1 = 0
   count2 = 0

   call setme
   call bar%a%cleanup()
   call bar%a%init()

   ! They should be called once
   if (count1 /= 23 .or. count2 /= 42) call abort ()

contains

   subroutine dummy1
      implicit none
      !print *, 'dummy1'
      count1 = 23 
   end subroutine dummy1

   subroutine dummy2
      implicit none
      !print *, 'dummy2'
      count2 = 42
   end subroutine dummy2
   
   subroutine setme
      use foo, only: bar, cg_ext
      implicit none
      procedure(cg_ext), pointer :: a_init, a_clean

      a_init => dummy1
      a_clean => dummy2
      call bar%epa_init(a_init, a_clean)
   end subroutine setme

end program ala

! { dg-final { scan-tree-dump-times "ext_ptr.\[0-9\]+.init = 0B;" 1 "original" } }
! { dg-final { scan-tree-dump-times "ext_ptr.\[0-9\]+.cleanup = 0B;" 1 "original" } }
! { dg-final { scan-tree-dump-times "ext_ptr.1.init = \\*init;" 1 "original" } }
! { dg-final { scan-tree-dump-times "ext_ptr.1.cleanup = \\*cleanup;" 1 "original" } }
! { dg-final { scan-tree-dump-times "this->_data->a.init = \\*init;" 1 "original" } }
! { dg-final { scan-tree-dump-times "this->_data->a.cleanup = \\*cleanup;" 1 "original" } }
! { dg-final { scan-tree-dump-times "ext_ptr.\[0-9\]+.init = this->_data->a.init;" 1 "original" } }
! { dg-final { scan-tree-dump-times "ext_ptr.\[0-9\]+.cleanup = this->_data->a.cleanup;" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
