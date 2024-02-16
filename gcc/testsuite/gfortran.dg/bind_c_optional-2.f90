! { dg-do run }
! PR fortran/113866
!
! Check interoperability of assumed-length character (optional and
! non-optional) dummies between bind(c) and non-bind(c) procedures

module bindcchar
  implicit none
  integer, parameter :: n = 100, l = 10
contains
  subroutine bindc_optional (c2, c4) bind(c)
    character(*), optional :: c2, c4(n)
!   print *, c2(1:3)
!   print *, c4(5)(1:3) 
    if (.not. present (c2) .or. .not. present (c4)) stop 8
    if (len (c2) /= l .or. len (c4) /= l) stop 81
    if (c2(1:3)    /= "a23") stop 1
    if (c4(5)(1:3) /= "bcd") stop 2
  end

  subroutine bindc (c2, c4) bind(c)
    character(*) :: c2, c4(n)
    if (len (c2) /= l .or. len (c4) /= l) stop 82
    if (c2(1:3)    /= "a23") stop 3
    if (c4(5)(1:3) /= "bcd") stop 4
    call bindc_optional (c2, c4)
  end

  subroutine not_bindc_optional (c1, c3)
    character(*), optional :: c1, c3(n)
    if (.not. present (c1) .or. .not. present (c3)) stop 5
    if (len (c1) /= l .or. len (c3) /= l) stop 83
    call bindc_optional (c1, c3)
    call bindc          (c1, c3)
  end

  subroutine not_bindc_optional_deferred (c5, c6)
    character(:), allocatable, optional :: c5, c6(:)
    if (.not. present (c5) .or. .not. present (c6)) stop 6
    if (len (c5) /= l .or. len (c6) /= l) stop 84
    call not_bindc_optional (c5, c6)
    call bindc_optional     (c5, c6)
    call bindc              (c5, c6)
  end

  subroutine not_bindc_optional2 (c7, c8)
    character(*), optional :: c7, c8(:)
    if (.not. present (c7) .or. .not. present (c8)) stop 7
    if (len (c7) /= l .or. len (c8) /= l) stop 85
    call bindc_optional (c7, c8)
    call bindc          (c7, c8)
  end

  subroutine bindc_optional2 (c2, c4) bind(c)
    character(*), optional :: c2, c4(n)
    if (.not. present (c2) .or. .not. present (c4)) stop 8
    if (len (c2) /= l .or. len (c4) /= l) stop 86
    if (c2(1:3)    /= "a23") stop 9
    if (c4(5)(1:3) /= "bcd") stop 10
    call bindc_optional     (c2, c4)
    call not_bindc_optional (c2, c4)
  end

  subroutine bindc_optional_missing (c1, c2, c3, c4, c5) bind(c)
    character(*), optional :: c1, c2(n), c3(:), c4(..), c5(*)
    if (present (c1)) stop 11
    if (present (c2)) stop 12
    if (present (c3)) stop 13
    if (present (c4)) stop 14
    if (present (c5)) stop 15
  end

  subroutine non_bindc_optional_missing (c1, c2, c3, c4, c5)
    character(*), optional :: c1, c2(n), c3(:), c4(..), c5(*)
    if (present (c1)) stop 21
    if (present (c2)) stop 22
    if (present (c3)) stop 23
    if (present (c4)) stop 24
    if (present (c5)) stop 25
  end
end module

program p
  use bindcchar
  implicit none
  character(l) :: a, b(n)
  character(:), allocatable :: d, e(:)
  a = 'a234567890'
  b = 'bcdefghijk'
  call not_bindc_optional (a, b)
  call bindc_optional (a, b)
  call not_bindc_optional2 (a, b)
  call bindc_optional2 (a, b)
  allocate (d, source=a)
  allocate (e, source=b)
  call not_bindc_optional (d, e)
  call bindc_optional (d, e)
  call not_bindc_optional2 (d, e)
  call bindc_optional2 (d, e)
  call not_bindc_optional_deferred (d, e)
  deallocate (d, e)
  call non_bindc_optional_missing ()
  call bindc_optional_missing ()
end
