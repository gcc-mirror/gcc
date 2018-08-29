! { dg-do run }
!
! PR fortran/59906
!
! Contributed by H Anlauf  <anlauf@gmx.de>
!
! Failed generate character scalar for scalarized loop for elemantal call.
!
program x
  implicit none
  call y('bbb')
contains

  subroutine y(str)
    character(len=*), intent(in) :: str
    character(len=len_trim(str)) :: str_aux
    character(len=3) :: str3 = 'abc'

    str_aux = str

    ! Compiled but did not give correct result
    if (any (str_cmp((/'aaa','bbb'/), str) .neqv. [.FALSE.,.TRUE.])) STOP 1

    ! Did not compile
    if (any (str_cmp((/'bbb', 'aaa'/), str_aux) .neqv. [.TRUE.,.FALSE.])) STOP 2

    ! Verify patch
    if (any (str_cmp((/'bbb', 'aaa'/), str3) .neqv. [.FALSE.,.FALSE.])) STOP 3
    if (any (str_cmp((/'bbb', 'aaa'/), 'aaa') .neqv. [.FALSE.,.TRUE.])) STOP 4

  end subroutine y

  elemental logical function str_cmp(str1, str2)
    character(len=*), intent(in) :: str1
    character(len=*), intent(in) :: str2
    str_cmp = (str1 == str2)
  end function str_cmp

end program x
