! { dg-do run }
! PR fortran/84615
! Charlen should always be the ABI defined character length type
! regardless of which kind it is declared as in the source.
program TestStringTools
  character(len=52)               :: txt
  character(len=1), dimension(52) :: chararr = &
       (/(char(i+64),char(i+96), i = 1,26)/)
  txt = chararray2string(chararr)
  if (txt .ne. "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz") &
       STOP 1
contains
  function chararray2string(chararray) result(text)
    character(len=1), dimension(:) :: chararray    ! input
    character(len=int(size(chararray, 1), kind=8)) :: text      ! output
    do i = 1,size(chararray,1)
       text(i:i) = chararray (i)
    end do
  end function chararray2string
end program TestStringTools
