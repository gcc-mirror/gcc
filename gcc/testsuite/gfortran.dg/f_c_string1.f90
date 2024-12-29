!
! { dg-do run }
!
! PR117643 F_C_STRING from F23 is missing.
! Test case and initial patch provided by Steve Kargl.
program foo

   use iso_c_binding, only : c_null_char, c_char, f_c_string, c_size_t

   implicit none

   logical asis
   character(len=6, kind=c_char) :: s1
   character(len=:, kind=c_char), allocatable :: s2

   interface
      !
      ! strlen() counts up to '\0', and excludes it from the count
      !
      function strlen(s) bind(c,name="strlen")
         import c_char, c_size_t
         integer(c_size_t) strlen
         character(len=1,kind=c_char), intent(in) :: s(*)
      end function strlen
   end interface

   s1 = 'abc   '
   s2 = f_c_string(s1)
   if (len_trim(s1) /= int(strlen(s2), 4)) stop 1

   s1 = ' ghij '
   s2 = f_c_string(s1)
   if (len_trim(s1) /= int(strlen(s2), 4)) stop 2

   s2 = f_c_string(s1, .true.)
   if (len(s1) /= int(strlen(s2), 4)) stop 3

   s2 = f_c_string(s1, .false.)
   if (len_trim(s1) /= int(strlen(s2), 4)) stop 4

   asis = .true.
   s2 = f_c_string(s1, asis)
   if (len(s1) /= int(strlen(s2), 4)) stop 5

   asis = .false.
   s2 = f_c_string(s1, asis)
   if (len_trim(s1) /= int(strlen(s2), 4)) stop 6

end program foo
