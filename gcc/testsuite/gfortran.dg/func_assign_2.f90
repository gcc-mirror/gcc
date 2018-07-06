! { dg-do run }
! Test the fix for PR40551 in which the assignment
! was not dealing correctly with non-contiguous lhs
! references; eg. a(1,:)
!
! Reported by by Maciej Zwierzycki
! at http://gcc.gnu.org/ml/fortran/2009-06/msg00254.html
! and by Tobias Burnus <burnus@gcc.gnu.org> on Bugzilla
!
integer :: a(2,2)
a = -42
a(1,:) = func()
if (any (reshape (a, [4]) /= [1, -42, 2, -42])) STOP 1
a = -42
a(2,:) = func()
if (any (reshape (a, [4]) /= [-42, 1, -42, 2])) STOP 2
a = -42
a(:,1) = func()
if (any (reshape (a, [4]) /= [1, 2, -42, -42])) STOP 3
a = -42
a(:,2) = func()
if (any (reshape (a, [4]) /= [-42, -42, 1, 2])) STOP 4
contains
 function func()
   integer :: func(2)
   call sub(func)
 end function func
 subroutine sub(a)
   integer :: a(2)
   a = [1,2]
 end subroutine
end

