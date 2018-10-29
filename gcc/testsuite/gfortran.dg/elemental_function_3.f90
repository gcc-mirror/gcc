! { dg-do run }
!
! Test the fix for PR84109 in which the call to the elemental function
! 'adjustl' was being added before the scalarization loop in the assignment.
! Since the result temporary was being declared in the loop body, this
! drove the gimplifier crazy. It is sufficient to compile this testcase
! since it used to ICE. This is the intrinsic counterpart to PR87239,
! which is tested for the absence of an ICE in elemental_function_2.f90.
! In this fix, a further improvement was to keep scalar calls outside the
! scalarization loop and this is tested with 'my_adjustl'.
!
! Contributed by Willem Vermin  <wvermin@gmail.com>
!
program prog
   implicit none
   character(len=:), allocatable :: c(:)
   integer :: cnt = 0

   allocate(character(len=20) :: c(10))
   c = "  ab  "
   c = adjustl(c)                        ! Used to ICE
   if (trim (c(1)) .ne. "ab") stop 1

   c = my_adjustl (" abcdefg ")
   if (trim (c(1)) .ne. "abcdefg") stop 2
   if (cnt .ne. 1) stop 3               ! Outside the scalarization loop
   if (size (c, 1) .ne. 10) stop 4
   if (len (c) .ne. 20) stop 5

   cnt = 0
   c = my_adjustl ([" uv ", " xy "])
   if (trim (c(2)) .ne. "xy") stop 6
   if (cnt .ne. size (c, 1)) stop 7     ! Inside the scalarization loop
   if (size (c, 1) .ne. 2) stop 8

contains

   impure elemental function my_adjustl(arg) result (res)
      character(*), intent(in) :: arg
      character(len = len (arg)) :: res
      res = adjustl (arg)
      cnt = cnt + 1                    ! Test how many calls are made
   end function
end program
