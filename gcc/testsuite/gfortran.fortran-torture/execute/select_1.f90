! from PR 15962, we used to require constant expressions instead of
! initialization expressions in case-statements
function x(k)
integer :: k
integer :: x
integer, parameter :: i(2) = (/1,2/)

select case(k)
case (1:size(i))
    x = i(k)
case default
    x = 0
end select
end function

if (x(2).NE.2 .OR. x(11).NE.0) call abort()
end
