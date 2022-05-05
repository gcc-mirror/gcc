! This test case used to ICE in verify_ssa due to the iterator variable j
! incorrectly being inserted into program scope.

program p
   integer :: i
   do i = 1, 3
      call sub (s(i))
   end do
contains
   function s(n) result(z)
      integer, target, intent(in) :: n
      integer, pointer :: z
      integer :: a(8), b(8), c(8)
      !$omp task affinity (iterator(j=1:8) : a(j), b(j), c(j))
      !$omp end task
      z => n
   end
end
