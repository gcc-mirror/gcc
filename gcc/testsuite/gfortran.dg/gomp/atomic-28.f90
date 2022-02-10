! { dg-do compile }
!
! PR fortran/104329
!
! Contributed by G. Steinmetz
!
subroutine z1
   character(:), allocatable :: x(:)
   x = ['123']
   !$omp atomic update
   x = (x)  ! { dg-error "OMP ATOMIC statement must set a scalar variable of intrinsic type" }
end

subroutine z2
   character(:), allocatable :: x(:)
   x = ['123']
   !$omp atomic update
   x = 'a' // x // 'e'  ! { dg-error "OMP ATOMIC statement must set a scalar variable of intrinsic type" }
end


subroutine z3
   character(:), allocatable :: x(:)
   x = ['123']
   !$omp atomic capture
   x = 'a' // x // 'e'  ! { dg-error "OMP ATOMIC statement must set a scalar variable of intrinsic type" }
   x = x
end
