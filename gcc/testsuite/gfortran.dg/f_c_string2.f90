! { dg-do run }
! See pr117643, this tests passing of the optional argument when it is
! not present. 

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
   asis = .true.
   call check (asis) ! OK
   asis = .false.
   call check (asis) ! OK
   call check ()     ! segfault fixed

 contains

  subroutine check (asis)
    logical, optional, intent(in) :: asis
  
    s2 = f_c_string(s1, asis)
    if (present(asis)) then
      if (asis) then
        if (int(strlen(s2)) /= 6) then
          stop 1
        endif
      else
        if (len_trim(s1) /= int(strlen(s2))) then
          stop 2
        endif
      endif
    endif
  end subroutine check

end program foo
