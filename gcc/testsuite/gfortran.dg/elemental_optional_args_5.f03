! { dg-do run }
!
! PR fortran/50981
! Test the handling of optional, polymorphic and non-polymorphic arguments
! to elemental procedures. 
!
! Original testcase by Tobias Burnus <burnus@net-b.de>

implicit none
type t
  integer :: a
end type t

type t2
  integer, allocatable :: a
  integer, allocatable :: a2(:)
  integer, pointer :: p => null()
  integer, pointer :: p2(:) => null()
end type t2

type(t), allocatable :: ta, taa(:)
type(t), pointer :: tp, tpa(:)
class(t), allocatable :: ca, caa(:)
class(t), pointer :: cp, cpa(:)

type(t2) :: x

integer :: s, v(2)

tp => null()
tpa => null()
cp => null()
cpa => null()

! =============== sub1 ==================
! SCALAR COMPONENTS: Non alloc/assoc

s = 3
v = [9, 33]

call sub1 (s, x%a, .false.)
call sub1 (v, x%a, .false.)
!print *, s, v
if (s /= 3) call abort()
if (any (v /= [9, 33])) call abort()

call sub1 (s, x%p, .false.)
call sub1 (v, x%p, .false.)
!print *, s, v
if (s /= 3) call abort()
if (any (v /= [9, 33])) call abort()


! SCALAR COMPONENTS: alloc/assoc

allocate (x%a, x%p)
x%a = 4
x%p = 5
call sub1 (s, x%a, .true.)
call sub1 (v, x%a, .true.)
!print *, s, v
if (s /= 4*2) call abort()
if (any (v /= [4*2, 4*2])) call abort()

call sub1 (s, x%p, .true.)
call sub1 (v, x%p, .true.)
!print *, s, v
if (s /= 5*2) call abort()
if (any (v /= [5*2, 5*2])) call abort()



contains

  elemental subroutine sub1 (x, y, alloc)
    integer, intent(inout) :: x
    integer, intent(in), optional :: y
    logical, intent(in) :: alloc
    if (alloc .neqv. present (y)) &
      x = -99
    if (present(y)) &
      x = y*2
  end subroutine sub1

end

