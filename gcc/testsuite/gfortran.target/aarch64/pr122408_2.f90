! { dg-do run }
! { dg-additional-options "-O2" }
! { dg-additional-options "-O2 -march=armv8.3-a" { target arm_v8_3a_complex_neon_hw } }

module util
  use iso_fortran_env, only: real64, int64
  implicit none
contains
  pure logical function bitwise_eq(x, y)
    complex(real64), intent(in) :: x, y
    integer(int64) :: xr, xi, yr, yi
    xr = transfer(real(x,kind=real64), 0_int64)
    xi = transfer(aimag(x),             0_int64)
    yr = transfer(real(y,kind=real64),  0_int64)
    yi = transfer(aimag(y),              0_int64)
    bitwise_eq = (xr == yr) .and. (xi == yi)
  end function bitwise_eq

  subroutine check_equal(tag, got, ref, nfail)
    character(*), intent(in) :: tag
    complex(real64), intent(in) :: got(:), ref(:)
    integer, intent(inout) :: nfail
    integer :: i
    do i = 1, size(got)
      if (.not. bitwise_eq(got(i), ref(i))) then
        nfail = nfail + 1
        write(*,'(A,": mismatch at i=",I0, "  got=",2ES16.8,"  ref=",2ES16.8)') &
             trim(tag), i, real(got(i)), aimag(got(i)), real(ref(i)), aimag(ref(i))
      end if
    end do
  end subroutine check_equal
end module util

module fcmla_ops
  use iso_fortran_env, only: real64
  implicit none
contains
  subroutine c_add_ab(n, a, c, b)         ! C += A * B
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
end module fcmla_ops

program fcmla_accum_pairs
  use iso_fortran_env, only: real64
  use util
  use fcmla_ops
  implicit none

  integer, parameter :: n = 4
  complex(real64) :: a, b(n), c0(n)
  complex(real64) :: c_add_ab_got(n),      c_add_ab_ref(n)
  complex(real64) :: c_sub_ab_got(n),      c_sub_ab_ref(n)
  complex(real64) :: c_add_conjb_got(n),   c_add_conjb_ref(n)
  complex(real64) :: c_sub_conjb_got(n),   c_sub_conjb_ref(n)
  integer :: i, fails

  ! Constants (include a signed-zero lane)
  a    = cmplx( 2.0_real64, -3.0_real64, kind=real64)
  b(1) = cmplx( 1.5_real64, -2.0_real64, kind=real64)
  b(2) = cmplx(-4.0_real64,  5.0_real64, kind=real64)
  b(3) = cmplx(-0.0_real64,  0.0_real64, kind=real64)
  b(4) = cmplx( 0.25_real64, 3.0_real64, kind=real64)

  c0(1) = cmplx( 1.0_real64, -2.0_real64, kind=real64)
  c0(2) = cmplx( 3.0_real64, -4.0_real64, kind=real64)
  c0(3) = cmplx(-5.0_real64,  6.0_real64, kind=real64)
  c0(4) = cmplx( 0.0_real64,  0.0_real64, kind=real64)

  ! Run each form
  c_add_ab_got    = c0; call c_add_ab     (n, a, c_add_ab_got,    b)
  c_sub_ab_got    = c0; call c_sub_ab     (n, a, c_sub_ab_got,    b)
  c_add_conjb_got = c0; call c_add_a_conjb(n, a, c_add_conjb_got, b)
  c_sub_conjb_got = c0; call c_sub_a_conjb(n, a, c_sub_conjb_got, b)

  ! Scalar references
  do i = 1, n
    c_add_ab_ref(i)    = c0(i) + a * b(i)
    c_sub_ab_ref(i)    = c0(i) - a * b(i)
    c_add_conjb_ref(i) = c0(i) + a * conjg(b(i))
    c_sub_conjb_ref(i) = c0(i) - a * conjg(b(i))
  end do

  ! Bitwise checks
  fails = 0
  call check_equal("C +=  A*B       ", c_add_ab_got,    c_add_ab_ref,    fails)
  call check_equal("C -=  A*B       ", c_sub_ab_got,    c_sub_ab_ref,    fails)
  call check_equal("C +=  A*conj(B) ", c_add_conjb_got, c_add_conjb_ref, fails)
  call check_equal("C -=  A*conj(B) ", c_sub_conjb_got, c_sub_conjb_ref, fails)

  if (fails == 0) then
    stop 0
  else
    stop 1
  end if
end program fcmla_accum_pairs

