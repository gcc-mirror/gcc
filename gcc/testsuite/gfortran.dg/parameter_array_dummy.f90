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
! { dg-output " *1 aa(\n|\r\n|\r)" }
! { dg-output " *2 ab(\n|\r\n|\r)" }
! { dg-output " *3 aaab(\n|\r\n|\r)" }
! { dg-output " *4 abaa(\n|\r\n|\r)" }
! { dg-output " *5 ababab(\n|\r\n|\r)" }
