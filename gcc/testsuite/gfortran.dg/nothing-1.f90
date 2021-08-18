module m
  implicit none (type, external)
  !$omp nothing

  type t
    !$omp nothing
    integer s
  end type

contains

integer function foo (i)
  integer :: i

  !$omp nothing
  if (.false.) &
& &    !$omp nothing
    i = i + 1

! In the following, '& & !$' is not a valid OpenMP sentinel and,
! hence, the line is regarded as comment
  if (.false.) &
&   & !$omp nothing
    then
  end if
  foo = i
end
end module
