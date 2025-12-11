! { dg-error ".* at \\(1\\) requires '-fopenmp-allocators'" "" { target *-*-* } 24 }
! { dg-warning "All files that might deallocate such a variable must be compiled with '-fopenmp-allocators'" "" { target *-*-* } 24 }
program test_deprecations
  integer :: i
  integer :: j
  integer, allocatable :: a(:)
  integer :: x(10)
  integer :: y, z

  ! { dg-warning "'to' clause with 'declare target' at \\(1\\) deprecated since OpenMP 5.2, use 'enter' \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* } 11 }
  !$omp declare target to(i)
  !$omp do ordered(1)
    do i = 1,10
      ! { dg-warning "'source' modifier with 'depend' clause at \\(1\\) deprecated since OpenMP 5.2, use with 'doacross' \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* } 15 }
      !$omp ordered depend(source)
        j = i
      ! { dg-warning "'sink' modifier with 'depend' clause at \\(1\\) deprecated since OpenMP 5.2, use with 'doacross' \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* } 18 }
      !$omp ordered depend(sink : i)
        j = i
    end do
  !$omp end do

  ! { dg-warning "The use of one or more 'allocate' directives with an associated 'allocate' statement at \\(1\\) is deprecated since OpenMP 5.2, use an 'allocators' directive \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* } 24 }
  !$omp allocate(a)
    allocate(a(100))
    do i = 1,100
      a(i) = i
    end do
    deallocate(a)


  ! { dg-warning "The specification of modifiers without comma separators for the 'map' clause at \\(1\\) has been deprecated since OpenMP 5.2 \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* } 33 }
  !$omp target map(close to: x)
    x = 1
  !$omp end target

  z = 1
  ! { dg-warning "'-' operator at \\(1\\) for reductions deprecated in OpenMP 5.2 \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* } 39 }
  !$omp parallel do reduction(-:z)
  do y = 1,10
      z = z - y
  end do

end program
