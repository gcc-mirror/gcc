! { dg-do compile }
! { dg-additional-options "-O2 -march=armv8.3-a" }

subroutine c_add_ab(n, a, c, b)         ! C += A * B
  use iso_fortran_env, only: real64
  implicit none
  !GCC$ ATTRIBUTES noinline :: c_add_ab
  integer, intent(in) :: n
  complex(real64), intent(in)    :: a
  complex(real64), intent(inout) :: c(*)
  complex(real64), intent(in)    :: b(*)
  integer :: k
  do k = 1, n
    c(k) = c(k) + a * b(k)
  end do
end subroutine c_add_ab

subroutine c_sub_ab(n, a, c, b)         ! C -= A * B
  use iso_fortran_env, only: real64
  implicit none
  !GCC$ ATTRIBUTES noinline :: c_sub_ab
  integer, intent(in) :: n
  complex(real64), intent(in)    :: a
  complex(real64), intent(inout) :: c(*)
  complex(real64), intent(in)    :: b(*)
  integer :: k
  do k = 1, n
    c(k) = c(k) - a * b(k)
  end do
end subroutine c_sub_ab

subroutine c_add_a_conjb(n, a, c, b)    ! C += A * conj(B)
  use iso_fortran_env, only: real64
  implicit none
  !GCC$ ATTRIBUTES noinline :: c_add_a_conjb
  integer, intent(in) :: n
  complex(real64), intent(in)    :: a
  complex(real64), intent(inout) :: c(*)
  complex(real64), intent(in)    :: b(*)
  integer :: k
  do k = 1, n
    c(k) = c(k) + a * conjg(b(k))
  end do
end subroutine c_add_a_conjb

subroutine c_sub_a_conjb(n, a, c, b)    ! C -= A * conj(B)
  use iso_fortran_env, only: real64
  implicit none
  !GCC$ ATTRIBUTES noinline :: c_sub_a_conjb
  integer, intent(in) :: n
  complex(real64), intent(in)    :: a
  complex(real64), intent(inout) :: c(*)
  complex(real64), intent(in)    :: b(*)
  integer :: k
  do k = 1, n
    c(k) = c(k) - a * conjg(b(k))
  end do
end subroutine c_sub_a_conjb

! { dg-final { scan-assembler-times {fcmla\s+v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d, #0} 2 } }
! { dg-final { scan-assembler-times {fcmla\s+v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d, #270} 2 } }
