! { dg-do compile }
! { dg-options "-Wall" }

! PR 57022: [4.7/4.8/4.9 Regression] Inappropriate warning for use of TRANSFER with arrays
! Contributed by William Clodius <wclodius@los-alamos.net>

subroutine transfers (test)

  use, intrinsic :: iso_fortran_env
  
  integer, intent(in) :: test

  integer(int8)  :: test8(8)  = 0
  integer(int16) :: test16(4) = 0
  integer(int32) :: test32(2) = 0
  integer(int64) :: test64    = 0

  select case(test)
  case(0)
    test64 = transfer(test8, test64)
  case(1)
    test64 = transfer(test16, test64)
  case(2)
    test64 = transfer(test32, test64)
  case(3)
    test8  = transfer(test64, test8, 8)
  case(4)
    test16 = transfer(test64, test16, 4)
  case(5)
    test32 = transfer(test64, test32, 2)
  end select

end subroutine


! PR 53685: surprising warns about transfer with explicit character range
! Contributed by Jos de Kloe <kloedej@knmi.nl>

subroutine mytest(byte_array,val)
  integer, parameter :: r8_ = Selected_Real_Kind(15,307)  ! = real*8
  character(len=1), dimension(16), intent(in) :: byte_array
  real(r8_),intent(out) :: val
  val = transfer(byte_array(1:8),val)    
end subroutine
