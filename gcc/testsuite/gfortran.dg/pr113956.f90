! { dg-do run }
! Test the fix for PR113956
! Contributed by David Binderman  <dcb314@hotmail.com>
module m
contains
  subroutine test_array_char(p, x)
    character(*), target  :: x(:)
    character(:), pointer :: p(:)
    p => x                       ! ICE
  end subroutine
end module

  use m
  character(:), allocatable, target :: chr(:)
  character(:), pointer :: p(:)
  chr = ["ab","cd"]
  call test_array_char (p, chr)
  if (loc (chr) .ne. loc (p)) stop 1
  if (len (p) .ne. 2) stop 2
  if (any (p .ne. chr)) stop 3
end
