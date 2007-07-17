! { dg-do run }
! Tests the fix for PR32665 in which the structure initializer at line
! 13 was getting the array length wrong by one and in which the automatic
! deallocation of a in 14 was occurring before the evaluation of the rhs.
!
! Contributed by Daniel Franke <dfranke@gcc.gnu.org>
!
  TYPE :: x
    INTEGER, ALLOCATABLE :: a(:)
  END TYPE
  TYPE(x) :: a

  a = x ((/ 1, 2, 3 /))                             ! This is also pr31320.
  a = x ((/ a%a, 4 /))
  if (any (a%a .ne. (/1,2,3,4/))) call abort ()
end
