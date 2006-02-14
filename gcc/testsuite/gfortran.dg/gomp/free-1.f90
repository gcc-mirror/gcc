! { dg-require-effective-target tls }

subroutine foo
integer, save :: i ! Some comment
!$omp threadpri&
      !$omp&vate (i)
i = 1
end subroutine
