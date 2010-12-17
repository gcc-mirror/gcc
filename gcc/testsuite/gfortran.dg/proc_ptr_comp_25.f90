! { dg-do compile }
!
! PR 46060: [F03] procedure pointer component referenced without argument list
!
! Contributed by Stephen J. Bespalko <sjbespa@comcast.net>

implicit none

abstract interface
  function name_func (ivar) result (res)
    integer, intent(in) :: ivar
    character(len=8) :: res
  end function name_func
end interface

type var_type
  procedure(name_func), nopass, pointer :: name
end type var_type

type(var_type) :: vars
character(len=8) name

name = vars%name   ! { dg-error "requires an argument list" }

end
