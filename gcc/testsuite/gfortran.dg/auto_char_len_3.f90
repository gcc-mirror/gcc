! { dg-do run }
! Test the fix for PR26257, in which the implicit reference to
! chararray in the main program call of chararray2string would
! cause a segfault in gfc_build_addr_expr.
!
! Based on the reduced testcase in the PR.
module chtest
contains
  function chararray2string(chararray) result(text)
    character(len=1), dimension(:) :: chararray    ! input
    character(len=size(chararray, 1)) :: text      ! output
    do i = 1,size(chararray,1)
      text(i:i) = chararray (i)
    end do
  end function chararray2string
end module chtest
program TestStringTools
  use chtest
  character(len=52)               :: txt
  character(len=1), dimension(52) :: chararr = &
        (/(char(i+64),char(i+96), i = 1,26)/)
  txt = chararray2string(chararr)
  if (txt .ne. "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz") &
        call abort ()
end program TestStringTools

! { dg-final { cleanup-modules "chtest" } }
