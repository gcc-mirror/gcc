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
  if (len(c(         3        : 5                )) /= 3) call abort ! Case  1
  if (len(c(     i*(i+1)      :     (i+1)*i + 2  )) /= 3) call abort ! Case  2
  if (len(c(     i*(i+1)      : 2 + (i+1)*i      )) /= 3) call abort ! Case  3
  if (len(c(     i*(i+1) + 2  :     (i+1)*i + 3  )) /= 2) call abort ! Case  4
  if (len(c( 2 + i*(i+1)      :     (i+1)*i + 3  )) /= 2) call abort ! Case  5
  if (len(c(     i*(i+1) + 2  : 3 + (i+1)*i      )) /= 2) call abort ! Case  6
  if (len(c( 2 + i*(i+1)      : 3 + (i+1)*i      )) /= 2) call abort ! Case  7
  if (len(c(     i*(i+1) - 1  :     (i+1)*i + 1  )) /= 3) call abort ! Case  8
  if (len(c(     i*(i+1) - 1  : 1 + (i+1)*i      )) /= 3) call abort ! Case  9
  if (len(c(     i*(i+1)      :     (i+1)*i -(-1))) /= 2) call abort ! Case 10
  if (len(c(     i*(i+1) +(-2):     (i+1)*i - 1  )) /= 2) call abort ! Case 11 
  if (len(c(     i*(i+1) + 2  :     (i+1)*i -(-4))) /= 3) call abort ! Case 12
  if (len(c(     i*(i+1) - 3  :     (i+1)*i - 1  )) /= 3) call abort ! Case 13
  if (len(c(13 - i*(i+1)      :15 - (i+1)*i      )) /= 3) call abort ! Case 14
  if (len(c(     i*(i+1) +(-1):     (i+1)*i      )) /= 2) call abort ! Case 15
  if (len(c(-1 + i*(i+1)      :     (i+1)*i      )) /= 2) call abort ! Case 16
  if (len(c(     i*(i+1) - 2  :     (i+1)*i      )) /= 3) call abort ! Case 17
  if (len(c(  (i-2)*(i-3)     :   (i-3)*(i-2)    )) /= 1)       call abort ! Case 18
end program main
! { dg-final { scan-tree-dump-times "_abort" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
