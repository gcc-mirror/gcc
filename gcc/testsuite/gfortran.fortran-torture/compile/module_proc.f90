! Check module procedures with arguments
module module_proc
contains
subroutine s(p)
  integer p
end subroutine
end module

program test
use module_proc
integer i
call s(i)
end program

