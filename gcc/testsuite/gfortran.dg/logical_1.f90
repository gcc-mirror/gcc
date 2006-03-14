! { dg-do run }
! PR middle-end/19543
program logical_1
  implicit none
  logical(1), parameter :: t1 = .TRUE., f1 = .FALSE.
  logical(2), parameter :: t2 = .TRUE., f2 = .FALSE.
  logical(4), parameter :: t4 = .TRUE., f4 = .FALSE.
  logical(8), parameter :: t8 = .TRUE., f8 = .FALSE.
  character*2 :: t(4), f(4)

  write(t(1),*) t1
  write(f(1),*) f1
  write(t(2),*) t2
  write(f(2),*) f2
  write(t(3),*) t4
  write(f(3),*) f4
  write(t(4),*) t8
  write(f(4),*) f8

  if (any(t .ne. " T")) call abort
  if (any(f .ne. " F")) call abort
end
