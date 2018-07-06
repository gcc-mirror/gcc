! PR 15963 -- checks character comparison in initialization expressions
character(8), parameter :: a(5) = (/ "H", "E", "L", "L", "O" /)
call x(a)
contains
subroutine x(a)
character(8), intent(in) :: a(:)
integer :: b(count(a < 'F'))
if (size(b) /= 1) STOP 1
end subroutine x
end
