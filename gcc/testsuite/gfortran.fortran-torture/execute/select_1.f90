! from PR 15962, we used to require constant expressions instead of
! initialization expressions in case-statements
function j(k)
integer :: k
integer :: j
integer, parameter :: i(2) = (/1,2/)

select case(k)
case (1:size(i))
    j = i(k)
case default
    j = 0
end select
end function

if (j(2).NE.2 .OR. j(11).NE.0) call abort()
end
