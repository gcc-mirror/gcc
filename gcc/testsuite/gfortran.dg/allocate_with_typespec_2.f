C { dg-do compile }
C
C Allocation of arrays with a type-spec specification with implicit none.
C
       subroutine implicit_none_test1

          implicit none

          real, allocatable :: x(:)
          real(4), allocatable :: x4(:)
          real(8), allocatable :: x8(:)
          double precision, allocatable :: d1(:)
          doubleprecision, allocatable :: d2(:)
          character, allocatable :: c1(:)
          character(len=4), allocatable :: c2(:)

          type a
             integer mytype
          end type a

          type(a), allocatable :: b(:)

          allocate(real :: x(1))
          allocate(real(4) :: x4(1))
          allocate(real(8) :: x8(1))
          allocate(double precision :: d1(1))
          allocate(doubleprecision :: d2(1))
          allocate(character :: c1(1))
          allocate(character(len=4) :: c2(1))
          allocate(a :: b(1))

       end
C
C Allocation of a scalar with a type-spec specification with implicit none
C
       subroutine implicit_none_test2

          implicit none

          real, allocatable :: x
          real(4), allocatable :: x4
          real(8), allocatable :: x8
          double precision, allocatable :: d1
          doubleprecision, allocatable :: d2
          character, allocatable :: c1
          character(len=4), allocatable :: c2

          type a
             integer mytype
          end type a

          type(a), allocatable :: b

          allocate(real :: x)
          allocate(real(4) :: x4)
          allocate(real(8) :: x8)
          allocate(double precision :: d1)
          allocate(doubleprecision :: d2)
          allocate(character :: c1)
          allocate(character(len=4) :: c2)
          allocate(a :: b)

       end subroutine implicit_none_test2
C
C Allocation of arrays with a type-spec specification with implicit none.
C
       subroutine implicit_test3

          real, allocatable :: x(:)
          real(4), allocatable :: x4(:)
          real(8), allocatable :: x8(:)
          double precision, allocatable :: d1(:)
          doubleprecision, allocatable :: d2(:)
          character, allocatable :: c1(:)
          character(len=4), allocatable :: c2(:)

          type a
             integer mytype
          end type a

          type(a), allocatable :: b(:)

          allocate(real :: x(1))
          allocate(real(4) :: x4(1))
          allocate(real(8) :: x8(1))
          allocate(double precision :: d1(1))
          allocate(doubleprecision :: d2(1))
          allocate(character :: c1(1))
          allocate(character(len=4) :: c2(1))
          allocate(a :: b(1))

       end
C
C Allocation of a scalar with a type-spec specification without implicit none
C
       subroutine implicit_test4

          real, allocatable :: x
          real(4), allocatable :: x4
          real(8), allocatable :: x8
          double precision, allocatable :: d1
          doubleprecision, allocatable :: d2
          character, allocatable :: c1
          character(len=4), allocatable :: c2

          type a
             integer mytype
          end type a

          type(a), allocatable :: b

          allocate(real :: x)
          allocate(real(4) :: x4)
          allocate(real(8) :: x8)
          allocate(double precision :: d1)
          allocate(doubleprecision :: d2)
          allocate(character :: c1)
          allocate(character(len=4) :: c2)
          allocate(a :: b)

       end
