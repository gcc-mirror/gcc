! { dg-do compile }
!
! PR fortran/33818
!

subroutine ExportZMX(lu)
  implicit none
  integer :: lu
  interface
    function LowerCase(str)
      character(*),intent(in) :: str
      character(len(str))     :: LowerCase
    end function LowerCase
  end interface
  character(*),parameter :: UNAME(1:1)=(/'XXX'/)
  write(lu,'(a)') 'UNIT '//UpperCase(UNAME(1))
  write(lu,'(a)') 'Unit '//LowerCase(UNAME(1))
entry ExportSEQ(lu)
contains
  function UpperCase(str) result(res)
    character(*),intent(in) :: str
    character(len(str)) res
    res=str
  end function
end
