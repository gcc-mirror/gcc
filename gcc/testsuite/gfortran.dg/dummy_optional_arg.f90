! { dg-do compile }
! PR fortran/45495
!
! Code originally submitted by Philip Mason <pmason at ricardo dot com>
!
function jack(aa)
   character(len=*), intent(in) :: aa
   optional :: aa
   character(len=len(aa)+1) :: jack ! { dg-error "cannot be OPTIONAL" }
   jack = ''
end function jack

function diane(aa)
   character(len=*), intent(out) :: aa
   character(len=len(aa)+1) :: diane
   diane = '012345678901'
   aa = 'abcdefghijklmn'
end function diane
