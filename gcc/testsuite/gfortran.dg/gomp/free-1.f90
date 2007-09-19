! { dg-require-effective-target tls_native }

subroutine foo
integer, save :: i ! Some comment
!$omp threadpri&
      !$omp&vate (i)
i = 1
end subroutine
