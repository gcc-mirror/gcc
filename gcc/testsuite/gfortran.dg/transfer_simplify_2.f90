! { dg-do run }
! { dg-options "-O2" }
! { dg-options "-O2 -mieee" { target alpha*-*-* } }
! Tests the fix for the meta-bug PR31237 (TRANSFER intrinsic)
! Exercises gfc_simplify_transfer a random walk through types and shapes
! and compares its results with the middle-end version that operates on
! variables.
!
  implicit none
  call integer4_to_real4
  call real4_to_integer8
  call integer4_to_integer8
  call logical4_to_real8
  call real8_to_integer4
  call integer8_to_real4
  call integer8_to_complex4
  call character16_to_complex8
  call character16_to_real8
  call real8_to_character2
  call dt_to_integer1
  call character16_to_dt
contains
  subroutine integer4_to_real4
    integer(4), parameter ::  i1 = 11111_4
    integer(4)            ::  i2 = i1
    real(4), parameter    ::  r1 = transfer (i1, 1.0_4)
    real(4)               ::  r2

    r2 = transfer (i2, r2);
    if (r1 .ne. r2) call abort ()
  end subroutine integer4_to_real4

  subroutine real4_to_integer8
    real(4), parameter    ::  r1(2) = (/3.14159_4, 0.0_4/)
    real(4)               ::  r2(2) = r1
    integer(8), parameter ::  i1 = transfer (r1, 1_8)
    integer(8)            ::  i2

    i2 = transfer (r2, 1_8);
    if (i1 .ne. i2) call abort ()
  end subroutine real4_to_integer8

  subroutine integer4_to_integer8
    integer(4), parameter ::  i1(2) = (/11111_4, 22222_4/)
    integer(4)            ::  i2(2) = i1
    integer(8), parameter ::  i3 = transfer (i1, 1_8)
    integer(8)            ::  i4

    i4 = transfer (i2, 1_8);
    if (i3 .ne. i4) call abort ()
  end subroutine integer4_to_integer8

  subroutine logical4_to_real8
    logical(4), parameter ::  l1(2) = (/.false., .true./)
    logical(4)            ::  l2(2) = l1
    real(8), parameter    ::  r1 = transfer (l1, 1_8)
    real(8)               ::  r2

    r2 = transfer (l2, 1_8);
    if (r1 .ne. r2) call abort ()
  end subroutine logical4_to_real8

  subroutine real8_to_integer4
    real(8), parameter    ::  r1 = 3.14159_8
    real(8)               ::  r2 = r1
    integer(4), parameter ::  i1(2) = transfer (r1, 1_4, 2)
    integer(4)            ::  i2(2)

    i2 = transfer (r2, i2, 2);
    if (any (i1 .ne. i2)) call abort ()
  end subroutine real8_to_integer4

  subroutine integer8_to_real4
    integer               ::  k
    integer(8), parameter ::  i1(2) = transfer ((/asin (1.0_8), log (1.0_8)/), 0_8)
    integer(8)            ::  i2(2) = i1
    real(4), parameter    ::  r1(4) = transfer (i1, (/(1.0_4,k=1,4)/))
    real(4)               ::  r2(4)

    r2 = transfer (i2, r2);
    if (any (r1 .ne. r2)) call abort ()
  end subroutine integer8_to_real4

  subroutine integer8_to_complex4
    integer               ::  k
    integer(8), parameter ::  i1(2) = transfer ((/asin (1.0_8), log (1.0_8)/), 0_8)
    integer(8)            ::  i2(2) = i1
    complex(4), parameter ::  z1(2) = transfer (i1, (/((1.0_4,2.0_4),k=1,2)/))
    complex(4)            ::  z2(2)

    z2 = transfer (i2, z2);
    if (any (z1 .ne. z2)) call abort ()
  end subroutine integer8_to_complex4

  subroutine character16_to_complex8
    character(16), parameter ::  c1(2) = (/"abcdefghijklmnop","qrstuvwxyz1234567890"/)
    character(16)            ::  c2(2) = c1
    complex(8), parameter    ::  z1(2) = transfer (c1, (1.0_8,1.0_8), 2)
    complex(8)               ::  z2(2)

    z2 = transfer (c2, z2, 2);
    if (any (z1 .ne. z2)) call abort ()
  end subroutine character16_to_complex8

  subroutine character16_to_real8
    character(16), parameter ::  c1 = "abcdefghijklmnop"
    character(16)            ::  c2 = c1
    real(8), parameter    ::  r1(2) = transfer (c1, 1.0_8, 2)
    real(8)               ::  r2(2)

    r2 = transfer (c2, r2, 2);
    if (any (r1 .ne. r2)) call abort ()
  end subroutine character16_to_real8

  subroutine real8_to_character2
    real(8), parameter    ::  r1 = 3.14159_8
    real(8)               ::  r2 = r1
    character(2), parameter ::  c1(4) = transfer (r1, "ab", 4)
    character(2)            ::  c2(4)

    c2 = transfer (r2, "ab", 4);
    if (any (c1 .ne. c2)) call abort ()
  end subroutine real8_to_character2

  subroutine dt_to_integer1
    integer, parameter    :: i1(4) = (/1_4,2_4,3_4,4_4/)
    real, parameter       :: r1(4) = (/1.0_4,2.0_4,3.0_4,4.0_4/)
    type :: mytype
      integer(4) :: i(4)
      real(4) :: x(4)
    end type mytype
    type (mytype), parameter :: dt1 = mytype (i1, r1)
    type (mytype)            :: dt2 = dt1
    integer(1), parameter :: i2(32) = transfer (dt1, 1_1, 32)
    integer(1)            :: i3(32)

    i3 = transfer (dt2, 1_1, 32);
    if (any (i2 .ne. i3)) call abort ()
  end subroutine dt_to_integer1

  subroutine character16_to_dt
    character(16), parameter ::  c1 = "abcdefghijklmnop"
    character(16)            ::  c2 = c1
    type :: mytype
      real(4) :: x(2)
    end type mytype

    type (mytype), parameter :: dt1(2) = transfer (c1, mytype ((/1.0,2.0,3.0,4.0/)), 2)
    type (mytype)            :: dt2(2)

    dt2 = transfer (c2, dt2);
    if (any (dt1(1)%x .ne. dt2(1)%x)) call abort ()
    if (any (dt1(2)%x .ne. dt2(2)%x)) call abort ()
  end subroutine character16_to_dt

end
