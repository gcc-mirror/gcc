! Program to test initialization expressions involving subobjects
program parameter_2
  implicit none
  type :: SS
     integer :: I
     integer :: J
  end type SS
  type :: TT
    integer :: N
    type (SS), dimension(2) :: o
  end type
  
  type (SS), parameter :: s =  SS (1, 2)
  type (TT), parameter :: t = TT(42, (/ SS(3, 4), SS(8, 9) /))

  integer, parameter :: a(2) = (/5, 10/)
  integer, parameter :: d1 = s%i
  integer, parameter :: d2 = a(2)
  integer, parameter :: d4 = t%o(2)%j

  integer q1, q2, q3, q4
  common /c1/q1(d1), q2(d2), q3(a(1)), q4(d4) ! legal
end
