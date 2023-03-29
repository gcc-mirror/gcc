! { dg-do run }
!
! PR fortran/107441
! Check that with -fcoarray=lib, coarray metadata arguments are passed
! in the right order to procedures.
!
! Contributed by M.Morin

program p
  integer :: ci[*]
  ci = 17
  call s(1, ci, "abcd")
contains
  subroutine s(ra, ca, c)
    integer :: ra, ca[*]
    character(*) :: c
    ca[1] = 13
    if (ra /= 1) stop 1
    if (this_image() == 1) then
      if (ca /= 13) stop 2
    else
      if (ca /= 17) stop 3
    end if
    if (len(c) /= 4) stop 4
    if (c /= "abcd") stop 5
  end subroutine s
end program p
