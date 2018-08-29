! { dg-do compile }
! { dg-options "-fopenmp -fcray-pointer" }
!
! PR fortran/43985

subroutine pete(A)
  real(8) :: A
  print *, 'pete got ',A
  if (A /= 3.0) STOP 1
end subroutine pete

       subroutine bob()
         implicit none
         real(8) peted
         pointer (ipeted, peted(*))
         integer(4) sz
         ipeted = malloc(5*8)
         peted(1:5) = [ 1.,2.,3.,4.,5.]
         sz = 3
!$omp parallel default(shared)
         call pete(peted(sz))
!$omp end parallel
         return
       end subroutine bob

call bob()
end
