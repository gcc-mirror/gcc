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


! ARRAY COMPONENTS: Non alloc/assoc

v = [9, 33]

call sub1 (v, x%a2, .false.)
!print *, v
if (any (v /= [9, 33])) call abort()

call sub1 (v, x%p2, .false.)
!print *, v
if (any (v /= [9, 33])) call abort()


! ARRAY COMPONENTS: alloc/assoc

allocate (x%a2(2), x%p2(2))
x%a2(:) = [84, 82]
x%p2    = [35, 58]

call sub1 (v, x%a2, .true.)
!print *, v
if (any (v /= [84*2, 82*2])) call abort()

call sub1 (v, x%p2, .true.)
!print *, v
if (any (v /= [35*2, 58*2])) call abort()


! =============== sub_t ==================
! SCALAR DT: Non alloc/assoc

s = 3
v = [9, 33]

call sub_t (s, ta, .false.)
call sub_t (v, ta, .false.)
!print *, s, v
if (s /= 3) call abort()
if (any (v /= [9, 33])) call abort()

call sub_t (s, tp, .false.)
call sub_t (v, tp, .false.)
!print *, s, v
if (s /= 3) call abort()
if (any (v /= [9, 33])) call abort()

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

  elemental subroutine sub_t(x, y, alloc)
    integer, intent(inout) :: x
    type(t), intent(in), optional :: y
    logical, intent(in) :: alloc
    if (alloc .neqv. present (y)) &
      x = -99
    if (present(y)) &
      x = y%a*2
  end subroutine sub_t

end

