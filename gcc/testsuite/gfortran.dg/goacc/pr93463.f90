! PR fortran/93463
! { dg-do compile { target fopenmp } }
! { dg-additional-options "-fopenmp" }

program pr93463
   integer :: i, x, y, z
   !$omp parallel do
   do i = 1, 4
      !$acc enter data create(x)	! { dg-error "ACC ENTER DATA directive cannot be specified within" }
      !$acc exit data copyout(x)	! { dg-error "ACC EXIT DATA directive cannot be specified within" }
      !$acc cache(y)			! { dg-error "ACC CACHE directive cannot be specified within" }
      !$acc wait(1)			! { dg-error "ACC WAIT directive cannot be specified within" }
      !$acc update self(z)		! { dg-error "ACC UPDATE directive cannot be specified within" }
   end do
end
