! { dg-do compile }
! PR fortran/33646
!
!

module BAR_MODULE
   implicit none
   private
   public    create_
   interface create_
      module procedure create
   end interface
   type system_type
       integer(kind=kind(1)) :: max_memory_used
   end type

contains

   subroutine create(self)
    type(system_type) :: self
      pointer :: self
      allocate(self)
   end subroutine

end

module FOO_MODULE
   use BAR_MODULE
   implicit none
   private
   public    create_
   interface create_
      module procedure create
   end interface

   public    create_copy_
   interface create_copy_
      module procedure create_copy
   end interface
contains

   subroutine create(self)
    character(*) :: self
      pointer :: self
      nullify(self)
      allocate(self)

      self = " "
   end subroutine

   subroutine create_copy(self,s)
    character(*) :: self
      pointer :: self
      character(*) :: s
      call create_(self)
   end subroutine
end

! { dg-final { cleanup-modules "bar_module foo_module" } }
