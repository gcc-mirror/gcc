! { dg-do run }
!
! PR 60357: [F08] structure constructor with unspecified values for allocatable components
!
! Contributed by Antony Lewis <antony@cosmologist.info>

Type A
  integer :: X = 1
  integer, allocatable :: y
  integer, allocatable :: z(:)
end type

Type(A) :: Me = A(X=1)

if (allocated(Me%y)) call abort
if (allocated(Me%z)) call abort

end
