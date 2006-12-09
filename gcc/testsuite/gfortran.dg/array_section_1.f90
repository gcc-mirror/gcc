! { dg-do run }
! { dg-options "-fbounds-check" }
! Tests the fix for PR30003, in which the 'end' of an array section
! would not be evaluated at all if it was on the lhs of an assignment
! or would be evaluated many times if bound checking were on.
!
! Contributed by Erik Edelmann <eedelmann@gcc.gnu.org>
!
    implicit none
    integer :: a(5), b(3), cnt

    b = [ 1, 2, 3 ]
! Check the lhs references
    cnt = 0
    a(bar(1):3) = b
    if (cnt /= 1) call abort ()
    cnt = 0
    a(1:bar(3)) = b
    if (cnt /= 1) call abort ()
    cnt = 0
    a(1:3:bar(1)) = b
    if (cnt /= 1) call abort ()
! Check the rhs references
    cnt = 0
    a(1:3) = b(bar(1):3)
    if (cnt /= 1) call abort ()
    cnt = 0
    a(1:3) = b(1:bar(3))
    if (cnt /= 1) call abort ()
    cnt = 0
    a(1:3) = b(1:3:bar(1))
    if (cnt /= 1) call abort ()
contains
    integer function bar(n)
        integer, intent(in) :: n
        cnt = cnt + 1
        bar = n
    end function bar
end
