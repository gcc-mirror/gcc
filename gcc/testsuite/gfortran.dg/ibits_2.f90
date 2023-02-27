! { dg-do run }
! { dg-additional-options "-fcheck=bits" }
! PR fortran/108937 - Intrinsic IBITS(I,POS,LEN) fails when LEN equals
!                     to BIT_SIZE(I)
! Contributed by saitofuyuki@jamstec.go.jp

program test_bits
  implicit none
  integer, parameter :: KT = kind (1)
  integer, parameter :: lbits = bit_size (0_KT)
  integer(kind=KT) :: x, y0, y1
  integer(kind=KT) :: p, l

  x = -1
  p = 0
  do l = 0, lbits
     y0 = ibits  (x, p, l)
     y1 = ibits_1(x, p, l)
     if (y0 /= y1) then
        print *, l, y0, y1
        stop 1+l
     end if
  end do
contains
  elemental integer(kind=KT) function ibits_1(I, POS, LEN) result(n)
    !! IBITS(I, POS, LEN) = (I >> POS) & ~((~0) << LEN)
    implicit none
    integer(kind=KT),intent(in) :: I
    integer,         intent(in) :: POS, LEN
    n = IAND (ISHFT(I, - POS), NOT(ISHFT(-1_KT, LEN)))
  end function ibits_1
end program test_bits
