! { dg-do compile }
!
! PR 49591: [OOP] Multiple identical specific procedures in type-bound operator not detected
!
! This is interpretation request F03/0018:
! http://www.j3-fortran.org/doc/meeting/195/11-214.txt
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module M1
  type T
    integer x
  contains
    procedure :: MyAdd_t => myadd
    generic :: operator(+) => myAdd_t
  end type T
  type X
    real q
  contains
    procedure, pass(b) :: MyAdd_x => myadd
    generic :: operator(+) => myAdd_x       ! { dg-error "is already present in the interface" }
  end type X
contains
  integer function MyAdd ( A, B )
    class(t), intent(in) :: A
    class(x), intent(in) :: B
    myadd = a%x + b%q
  end function MyAdd
end module

module M2
  interface operator(+)
    procedure MyAdd
  end interface
  type T
    integer x
  contains
    procedure :: MyAdd_t => myadd
    generic :: operator(+) => myAdd_t  ! { dg-error "is already present in the interface" }
  end type T
contains
  integer function MyAdd ( A, B )
    class(t), intent(in) :: A
    real, intent(in) :: B
    myadd = a%x + b
  end function MyAdd
end module

! { dg-final { cleanup-modules "M1 M2" } }
