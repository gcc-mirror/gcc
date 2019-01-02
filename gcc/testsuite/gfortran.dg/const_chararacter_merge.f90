! { dg-do compile }
! { dg-options "-Os" }
! PR 48543
program main
  character(len=17) :: a
  character(len=34) :: b
  a = 'Supercalifragilis'
  b = 'Supercalifragilisticexpialidocious'
  print *,a," ",b
end program main
! { dg-final { scan-assembler-times "Supercalifragilis" 1 } }
