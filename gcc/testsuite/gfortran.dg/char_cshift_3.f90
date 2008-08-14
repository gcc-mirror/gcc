! { dg-do run }
! PR 36886 - misalignment of characters for cshift could cause
! problems on some architectures.
program main
  character(len=2) :: c2
  character(len=4), dimension(2,2) :: a, b, c, d
  ! Force misalignment of a or b
  common /foo/ a, c, c2, b, d
  a = 'aa'
  b = 'bb'
  d = cshift(b,1)
  c = cshift(a,1)
end program main
