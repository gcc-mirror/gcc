! { dg-do run }
! { dg-additional-options "-fcheck=bounds -g" }
! { dg-output "At line 18 .*" }
! { dg-shouldfail "Different CHARACTER lengths (32/0) in array constructor" }
!
! PR fortran/70231 - CHARACTER lengths in array constructors

program p
  implicit none
  integer, parameter  :: char_len = 32
  integer             :: l = 0
  character(char_len) :: ch = "a"
  character(char_len), allocatable :: ch_array(:), res1(:), res2(:)

  allocate(ch_array(0))
  res1 = [ ch_array, ch ]               ! was false positive
  print *, res1
  res2 = [[ch_array, ch(1:l)], ch(1:l)] ! was false negative on x86
  print *, res2
end
