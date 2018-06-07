! { dg-do run }
! { dg-options "-fdump-tree-original" }
! Test that all string length calculations are
! optimized away.
program main
  character (len=999) :: c
  character (len=5) :: unit
  unit = '    '
  read (unit=unit,fmt='(I5)') i   ! Hide from optimizers
  j = 7
  c = '123456789'
  if (len(c(         3        : 5                )) /= 3) STOP 1! Case  1
  if (len(c(     i*(i+1)      :     (i+1)*i + 2  )) /= 3) STOP 2! Case  2
  if (len(c(     i*(i+1)      : 2 + (i+1)*i      )) /= 3) STOP 3! Case  3
  if (len(c(     i*(i+1) + 2  :     (i+1)*i + 3  )) /= 2) STOP 4! Case  4
  if (len(c( 2 + i*(i+1)      :     (i+1)*i + 3  )) /= 2) STOP 5! Case  5
  if (len(c(     i*(i+1) + 2  : 3 + (i+1)*i      )) /= 2) STOP 6! Case  6
  if (len(c( 2 + i*(i+1)      : 3 + (i+1)*i      )) /= 2) STOP 7! Case  7
  if (len(c(     i*(i+1) - 1  :     (i+1)*i + 1  )) /= 3) STOP 8! Case  8
  if (len(c(     i*(i+1) - 1  : 1 + (i+1)*i      )) /= 3) STOP 9! Case  9
  if (len(c(     i*(i+1)      :     (i+1)*i -(-1))) /= 2) STOP 10! Case 10
  if (len(c(     i*(i+1) +(-2):     (i+1)*i - 1  )) /= 2) STOP 11! Case 11 
  if (len(c(     i*(i+1) + 2  :     (i+1)*i -(-4))) /= 3) STOP 12! Case 12
  if (len(c(     i*(i+1) - 3  :     (i+1)*i - 1  )) /= 3) STOP 13! Case 13
  if (len(c(13 - i*(i+1)      :15 - (i+1)*i      )) /= 3) STOP 14! Case 14
  if (len(c(     i*(i+1) +(-1):     (i+1)*i      )) /= 2) STOP 15! Case 15
  if (len(c(-1 + i*(i+1)      :     (i+1)*i      )) /= 2) STOP 16! Case 16
  if (len(c(     i*(i+1) - 2  :     (i+1)*i      )) /= 3) STOP 17! Case 17
  if (len(c(  (i-2)*(i-3)     :   (i-3)*(i-2)    )) /= 1)       STOP 18! Case 18
end program main
! { dg-final { scan-tree-dump-times "_gfortran_stop" 0 "original" } }
