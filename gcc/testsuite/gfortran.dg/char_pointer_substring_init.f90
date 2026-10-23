! { dg-do run }
!
! PR fortran/125763
!
! A scalar character pointer initialized to a substring of a target at
! declaration used to ICE in gfc_get_symbol_decl because the substring's
! character length was never resolved.

module m
  implicit none
  character(len=5), target :: t = "abcde"
  character(kind=4,len=5), target :: w = 4_"abcde"
contains
  subroutine check ()
    ! Deferred-length pointers to substrings.
    character(len=:), pointer :: pd1 => t(2:4)
    character(len=:), pointer :: pd2 => t(1:5)
    character(len=:), pointer :: pd3 => t(4:3)   ! zero length
    ! Explicit-length pointers to substrings.
    character(len=3), pointer :: pe1 => t(2:4)
    ! Wide-character pointers to substrings.
    character(kind=4,len=:), pointer :: pw1 => w(2:4)
    character(kind=4,len=3), pointer :: pw2 => w(3:5)

    if (len (pd1) /= 3 .or. pd1 /= "bcd") stop 1
    if (len (pd2) /= 5 .or. pd2 /= "abcde") stop 2
    if (len (pd3) /= 0) stop 3
    if (len (pe1) /= 3 .or. pe1 /= "bcd") stop 4
    if (len (pw1) /= 3 .or. pw1 /= 4_"bcd") stop 5
    if (len (pw2) /= 3 .or. pw2 /= 4_"cde") stop 6
  end subroutine
end module

program p
  use m
  call check ()
end program p
