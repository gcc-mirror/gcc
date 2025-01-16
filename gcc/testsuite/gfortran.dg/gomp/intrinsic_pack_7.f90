! PR fortran/118441

subroutine sub(s)
  character(len=*), intent(inout) :: s(:)
  integer :: n
  s( : ) =       s(:)                     ! OK
  n      = count(s(:) /= '')
  s(1:n) = pack (s(:), mask=(s(:) /= '')) ! ICE
end subroutine sub
