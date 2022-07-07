! { dg-do compile }
! { dg-options "-fopenmp" }

module m
  implicit none (type, external)

  integer i

  interface
    integer function s1 (x, y, z)
      integer, value :: x, y, z
      !$omp declare simd linear (x : val, step (1), val)  ! { dg-error "Multiple 'linear' modifiers specified" }
    end

    integer function s2 (x, y, z)
      integer, value :: x, y, z
      !$omp declare simd linear (x : val, step (1), step(2))  ! { dg-error "Multiple 'step' modifiers specified" }
    end

    integer function s3 (x, y, z)
      integer, value :: x, y, z
      !$omp declare simd linear (x : val, ref, step(2))  ! { dg-error "Multiple 'linear' modifiers specified" }
    end

  end interface

end module
