! { dg-do compile }
! { dg-options "-O2 -fpeel-loops -finline-functions -fipa-cp-clone -fdump-ipa-inline-details" }

module TensorProducts
  use, intrinsic :: iso_fortran_env

  implicit none

  integer, parameter :: dp = real64 ! KIND for double precision

  type Vect3D
    real(dp) :: x, y, z
  end type

contains

  type(Vect3D) pure function MySum(array)
    type(Vect3D), intent(in) :: array(:,:)

    mysum = Vect3D(sum(array%x), sum(array%y), sum(array%z))
  end function

  pure subroutine GenerateGrid(N, M, width, height, centre, P)
    integer,      intent(in)  :: N, M
    real(dp),     intent(in)  :: width, height
    type(Vect3D), intent(in)  :: centre
    type(Vect3D), intent(out) :: P(N, M)
    real(dp)                  :: x(N), y(M)
    integer                   :: i, j

    x = ([( i, i = 0, N-1 )] * width/(N-1)) - (width / 2) + centre%x
    y = ([( j, j = 0, M-1 )] * height/(M-1)) - (height / 2) + centre%y
    do concurrent (i = 1:N)
      do concurrent (j = 1:M)
        P(i, j) = Vect3D(x(i), y(j), centre%z)
      end do
    end do
    P(2:3,2:3)%z = P(2:3,2:3)%z + 1.0_dp*reshape([2,1,1,-2], [2,2])
  end subroutine

  type(Vect3D) pure function TP_SUM(NU, D, NV) result(tensorproduct)
    ! (NU) D (NV)^T, row * matrix * column
    ! TODO (#6): TensorProduct: Investigate whether using DO loops triggers a temporary array.
    ! copied from Surfaces
    real(dp),     intent(in) :: NU(4), NV(4)
    type(Vect3D), intent(in) :: D(4,4)
    integer                  :: i, j
    type(Vect3D)             :: P(4,4)

    do concurrent (i = 1:4)
      do concurrent (j = 1:4)
        P(i,j)%x = NU(i) * D(i,j)%x * NV(j)
        P(i,j)%y = NU(i) * D(i,j)%y * NV(j)
        P(i,j)%z = NU(i) * D(i,j)%z * NV(j)
      end do
    end do
    tensorproduct = MySum(P)
  end function

  subroutine RandomSeed()
    integer                                 :: seed_size, clock, i
    integer,              allocatable, save :: seed(:)

    if (.not. allocated(seed)) then
      call random_seed(size=seed_size)
      allocate(seed(seed_size))
      call system_clock(count=clock)
      seed = clock + 37 * [( i -1, i = 1, seed_size )]
      call random_seed(put=seed)
    end if
  end subroutine

  subroutine RunTPTests()
    type(Vect3D)       :: tp, P(4,4)
    integer, parameter :: i_max = 10000000
    real(dp)           :: NU(4,i_max), NV(4,i_max)
    real(dp)           :: sum
    real               :: t(2)
    integer            :: i

!    print *, 'This code variant uses explicit %x, %y and %z to represent the contents of Type(Vect3D).'
    call GenerateGrid(4, 4, 20.0_dp, 20.0_dp, Vect3D(0.0_dp,0.0_dp,20.0_dp), P)
    call RandomSeed()
!    call cpu_time(t(1))
    do i = 1, 4
      call random_number(NU(i,:))
      call random_number(NV(i,:))
    end do
!    call cpu_time(t(2))
!    print *, 'Random Numbers, time:  ', t(2)-t(1)
    sum = 0.0
    call cpu_time(t(1))
    do i = 1, i_max
      tp = TP_SUM(NU(:,i), P(1:4,1:4), NV(:,i))
      sum = sum + tp%x
    end do
    call cpu_time(t(2))
    print *, 'Using SUM, time:       ', t(2)-t(1)
    print *, 'sum =', sum
  end subroutine

  end module

  program Main
  use TensorProducts

  implicit none

  call RunTPTests()
  end program
! See PR88711. Inliner is currently not able to figure out that inlining tp_sum is a good idea.
! { dg-final { scan-ipa-dump "Inlined tp_sum/\[0-9\]+ into runtptests/\[0-9\]+" "inline" { xfail *-*-* } } }
