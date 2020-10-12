! { dg-do compile }
! PR96686: MIN/MAX should reject character arguments of different kind

program p
  implicit none
  character(kind=1) :: c1 = "1"
  character(kind=4) :: c4 = 4_"4"
  print *, min (c1, c4) ! { dg-error "Different character kinds" }
  print *, min (c4, c1) ! { dg-error "Different character kinds" }
end program p
