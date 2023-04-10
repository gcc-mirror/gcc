! { dg-do run }
!
! Test the fix for PR84472 in which the finalizations around the
! assignment in 'mymain' were not happening.
!
! Contributed by Vipul Parekh  <fortranfan@outlook.com>
!
module m

   use, intrinsic :: iso_fortran_env, only : output_unit

   implicit none

!   private

   integer, public :: final_counts = 0
   integer, public :: assoc_counts = 0

   type :: t
      private
      character(len=:), pointer :: m_s => null()
   contains
      private
      final :: final_t
      procedure, pass(this), public :: clean => clean_t
      procedure, pass(this), public :: init => init_t
      procedure, public :: assign_t
      generic, public :: ASSIGNMENT(=) => assign_t
   end type

   interface t
      module procedure :: construct_t
   end interface

   public :: t, assign_t

contains

   impure elemental subroutine assign_t (to, from)
     class(t), intent(out) :: to
     class(t), intent(in) :: from
     if (associated (from%m_s)) then
        allocate(to%m_s, source = from%m_s)
     else
        allocate(to%m_s, source = "new")
     endif
   end subroutine assign_t

   function construct_t( name ) result(new_t)

      ! argument list
      character(len=*), intent(in), optional :: name
      ! function result
      type(t) :: new_t

      if ( present(name) ) then
         call new_t%init( name )
      end if

   end function

   subroutine final_t( this )

      ! argument list
      type(t), intent(inout) :: this

      final_counts = final_counts + 1
      if ( associated(this%m_s) ) then
         assoc_counts = assoc_counts + 1
      endif
      call clean_t( this )

   end subroutine

   subroutine clean_t( this )

      ! argument list
      class(t), intent(inout) :: this

      if ( associated(this%m_s) ) then
         print *, this%m_s
         deallocate( this%m_s )
      end if
      this%m_s => null()

   end subroutine

   subroutine init_t( this, mname )

      ! argument list
      class(t), intent(inout)      :: this
      character(len=*), intent(in) :: mname

      call this%clean()
      allocate(character(len(mname)) :: this%m_s)
      this%m_s = mname

   end subroutine

end module
   use m, only : final_counts, assoc_counts
   call mymain
! See comment below.
   if (final_counts /= 3) stop 1
   if (assoc_counts /= 2) stop 2

contains
   subroutine mymain

   use m, only : t

   implicit none

   character(3), allocatable, target :: myname

   type(t) :: foo

   call foo%init( mname="123" )

   myname = "foo"
   foo = t( myname )

   call foo%clean()

! NAGFOR has assoc_counts =2, which is probably correct. If nullification
! of the pointer component is not done in gfortran, function finalization
! results in a double free. TODO fix this.
   if (final_counts /= 2) stop 3
   if (assoc_counts /= 2) stop 4
   end
end

