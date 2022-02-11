! This program used to ICE in convert_nonlocal_reference_op due to
! incorrect scoping of AFFINITY clause iterator variables.

program p
   integer :: a(8)
   !$omp task affinity (iterator(j=1:8) : a(j))
   !$omp end task
contains
   integer function f(x)
      class(*) :: x
   end
end
