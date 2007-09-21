! { dg-do run }
! { dg-options "-finit-character=32" }

program init_flag_6
  call char_test
end program init_flag_6

! Test some initializations for both implicitly and
! explicitly declared local variables.
subroutine char_test
  character*1 c1
  character*8 c2, c3(5)
  character c4(10)
  if (c1 /= ' ') call abort
  if (c2 /= '        ') call abort
  if (c3(1) /= '        ') call abort
  if (c3(5) /= '        ') call abort
  if (c4(5) /= ' ') call abort
end subroutine char_test
        
