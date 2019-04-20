! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_9.c }
!
! Fix a problem with CFI_is_contiguous
!
! Contributed by Gilles Gouaillardet  <gilles@rist.or.jp>
!
module cdesc
  interface
  function cdesc_f08(buf, expected) result (res) BIND(C, name="cdesc_c")
      USE, INTRINSIC :: ISO_C_BINDING
      implicit none
      INTEGER(C_INT) :: res
      type(*), dimension(..), INTENT(IN) :: buf
      integer(kind=kind(loc(res))),INTENT(IN) :: expected
    end function cdesc_f08
  end interface
end module

program cdesc_test
  use cdesc
  implicit none
  integer :: a0, a1(10), a2(10,10), a3(10,10,10)
  if (cdesc_f08(a0, LOC(a0)) .ne. 1) stop 1
  if (cdesc_f08(a1, LOC(a1(1))) .ne. 1) stop 2
  if (cdesc_f08(a2, LOC(a2(1,1))) .ne. 1) stop 3
  if (cdesc_f08(a3, LOC(a3(1,1,1))) .ne. 1) stop 4
end program
