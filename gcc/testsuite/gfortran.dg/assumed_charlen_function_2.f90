! { dg-do compile }
! Tests the fix for PR25416, which ICED in gfc_conv_function_call, when
! treating SPREAD in the statement below.
!
! Contributed by Ulrich Weigand  <uweigand@gcc.gnu.org>
function bug(self,strvec) result(res)
  character(*) :: self
  character(*), dimension(:), intent(in) :: strvec
  logical(kind=kind(.true.)) :: res

  res = any(index(strvec,spread(self,1,size(strvec))) /= 0)
end function

