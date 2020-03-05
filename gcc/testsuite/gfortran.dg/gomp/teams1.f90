! PR fortran/92756

program pr92756
  integer :: i
  !$omp teams distribute parallel do	! { dg-error "'teams' construct not closely nested inside of 'target' construct" }
  do i = 1, 64
  end do
end
