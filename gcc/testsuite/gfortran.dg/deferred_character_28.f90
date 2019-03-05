! { dg-do run }
!
! Test the fix for PR80931, which was nearly fix by the patch for PR87151.
! However, the 'span' for 'temp' was not being set and so a segfault
! occurred in the assignment at line 39.
!
! Contributed by Tiziano Mueller  <dev-zero@gentoo.org>
!
module input_section_types
   type :: section
      character(len=:), allocatable :: keywords_(:)

      contains
         procedure, pass :: add_keyword
   end type

   interface section
      procedure constructor
   end interface

contains

   type(section) function constructor ()
      allocate (character(len=255) :: constructor%keywords_(0))
   end function

   subroutine add_keyword (this, name)
      class(section), intent(inout) :: this
      character(*), intent(in)      :: name
      character(len=:), allocatable :: temp(:)

      integer :: n_elements

      n_elements = size (this%keywords_)
      allocate (character(len=255) :: temp(n_elements+1))
      temp(:n_elements) = this%keywords_
      call move_alloc (temp, this%keywords_)

      this%keywords_(n_elements+1) = name
   end subroutine
end module

   use input_section_types
   type(section) :: s
   character(*), parameter :: hello = "Hello World"
   character(*), parameter :: bye = "Goodbye World"

   s = constructor ()

   call s%add_keyword (hello)
   if (len (s%keywords_) .ne. 255) stop 1
   if (size (s%keywords_, 1) .ne. 1) stop 2
   if (trim (s%keywords_(1)) .ne. hello) stop 3

   call s%add_keyword (bye)
   if (len (s%keywords_) .ne. 255) stop 4
   if (size (s%keywords_, 1) .ne. 2) stop 5
   if (trim (s%keywords_(1)) .ne. hello) stop 6
   if (trim (s%keywords_(2)) .ne. bye) stop 7
end
