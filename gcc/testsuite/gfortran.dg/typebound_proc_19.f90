! { dg-do compile }
!
! PR fortran/47399
!
! Contributed by Wolfgang Kilian.
!

module mytypes
   implicit none
   private
   public :: mytype, get_i

   integer, save :: i_priv = 13
   type :: mytype
      integer :: dummy
    contains
      procedure, nopass :: i => get_i
   end type mytype
 contains
   pure function get_i () result (i)
     integer :: i
     i = i_priv
   end function get_i
end module mytypes

subroutine test()
   use mytypes
   implicit none

   type(mytype) :: a
   type(mytype), parameter :: a_const = mytype (0)
   integer, dimension (get_i()) :: x            ! #1
   integer, dimension (a%i()) :: y              ! #2
   integer, dimension (a_const%i()) :: z        ! #3

   if (size (x) /= 13 .or. size(y) /= 13 .or. size(z) /= 13) call abort()
!   print *, size (x), size(y), size(z)
end subroutine test

call test()
end

! { dg-final { cleanup-modules "mytypes" } }
