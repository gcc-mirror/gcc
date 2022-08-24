! { dg-do run }
! PR fortran/66193 - ICE for initialisation of some non-zero-sized arrays
! Testcase by G.Steinmetz

program p
  implicit none
  call s1
  call s2
  call s3
  call s4
contains
  subroutine s1
    integer(8), parameter :: z1(2) = 10 + [ integer(8) :: [ integer(4) ::],1,2]
    integer(8)            :: z2(2) = 10 + [ integer(8) :: [ integer(4) ::],1,2]
    integer(8)            :: z3(2)
    z3 = 10 + [ integer(8) :: [ integer(4) :: ], 1, 2 ]
    if ( z1(1) /= 11 .or. z1(2) /= 12 ) stop 1
    if ( z2(1) /= 11 .or. z2(2) /= 12 ) stop 2
    if ( z3(1) /= 11 .or. z3(2) /= 12 ) stop 3
  end subroutine s1

  subroutine s2
    logical(8), parameter :: z1(3) = .true. .or. &
         [ logical(8) :: [ logical(4) :: ], .false., .false., .true. ]
    logical(8)            :: z2(3) = .true. .or. &
         [ logical(8) :: [ logical(4) :: ], .false., .false., .true. ]
    logical(8)            :: z3(3)
    z3 = .true. .or. &
         [ logical(8) :: [ logical(4) :: ], .false., .false., .true. ]
    if ( .not. all(z1) ) stop 11
    if ( .not. all(z2) ) stop 12
    if ( .not. all(z3) ) stop 13
  end subroutine s2

  subroutine s3
    real(8), parameter :: eps = 4.0_8 * epsilon(1.0_8)
    real(8), parameter :: z1(2) = 10. + [ real(8) :: [ real(4) :: ], 1., 2. ]
    real(8)            :: z2(2) = 10. + [ real(8) :: [ real(4) :: ], 1., 2. ]
    real(8)            :: z3(2)
    z3 = 10.0 + [ real(8) :: [ real(4) :: ], 1.0, 2.0 ]

    if ( abs(1-z1(1)/11) > eps ) stop 21
    if ( abs(1-z1(2)/12) > eps ) stop 22
    if ( abs(1-z2(1)/11) > eps ) stop 23
    if ( abs(1-z2(2)/12) > eps ) stop 24
    if ( abs(1-z3(1)/11) > eps ) stop 25
    if ( abs(1-z3(2)/12) > eps ) stop 26
  end subroutine s3

  subroutine s4
    real, parameter :: x(3) = 2.0 * [real :: 1, (2), 3]
    real, parameter :: y(2) =       [real :: 1, (2)] + 10.0
    real, parameter :: z(2) =       [real ::(1),(2)] + 10.0
  end subroutine s4
end program p
