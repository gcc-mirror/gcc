! { dg-do run }
! PR fortran/31188
program foo_mod
  implicit none
  character (len=1), parameter :: letters(2) = (/"a","b"/)
  call concat(1, [1])
  call concat(2, [2])
  call concat(3, [1,2])
  call concat(4, [2,1])
  call concat(5, [2,2,2])
contains
  subroutine concat(i, ivec)
    integer, intent(in)  :: i, ivec(:)
    write (*,*) i, "a" // letters(ivec)
  end subroutine concat
end program foo_mod
! { dg-output " *1 aa(\r*\n+)" }
! { dg-output " *2 ab(\r*\n+)" }
! { dg-output " *3 aaab(\r*\n+)" }
! { dg-output " *4 abaa(\r*\n+)" }
! { dg-output " *5 ababab(\r*\n+)" }
