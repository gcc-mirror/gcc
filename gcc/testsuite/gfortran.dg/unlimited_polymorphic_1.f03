! { dg-do run }
!
! Basic tests of functionality of unlimited polymorphism
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
MODULE m
  TYPE :: a
    integer :: i
  END TYPE

contains
  subroutine bar (arg, res)
    class(*) :: arg
    character(100) :: res
    select type (w => arg)
      type is (a)
        write (res, '(a, I4)') "type(a)", w%i
      type is (integer)
        write (res, '(a, I4)') "integer", w
      type is (real(4))
        write (res, '(a, F4.1)') "real4", w
      type is (real(8))
        write (res, '(a, F4.1)') "real8", w
      type is (character(*, kind = 4))
        STOP 1
      type is (character(*))
        write (res, '(a, I2, a, a)') "char(", LEN(w), ")", trim(w)
    end select
  end subroutine

  subroutine foo (arg, res)
    class(*) :: arg (:)
    character(100) :: res
    select type (w => arg)
      type is (a)
        write (res,'(a, 10I4)') "type(a) array", w%i
      type is (integer)
        write (res,'(a, 10I4)') "integer array", w
      type is (real)
        write (res,'(a, 10F4.1)') "real array", w
      type is (character(*))
        write (res, '(a5, I2, a, I2, a1, 2(a))') &
               "char(",len(w),",", size(w,1),") array ", w
    end select
  end subroutine
END MODULE


  USE m
  TYPE(a), target :: obj1 = a(99)
  TYPE(a), target :: obj2(3) = a(999)
  integer, target :: obj3 = 999
  real(4), target :: obj4(4) = [(real(i), i = 1, 4)]
  integer, target :: obj5(3) = [(i*99, i = 1, 3)]
  class(*), pointer :: u1
  class(*), pointer :: u2(:)
  class(*), allocatable :: u3
  class(*), allocatable :: u4(:)
  type(a), pointer :: aptr(:)
  character(8) :: sun = "sunshine"
  character(100) :: res

 ! NULL without MOLD used to cause segfault
  u2 => NULL()
  u2 => NULL(aptr)

! Test pointing to derived types.
  u1 => obj1
  if (SAME_TYPE_AS (obj1, u1) .neqv. .TRUE.) STOP 1
  u2 => obj2
  call bar (u1, res)
  if (trim (res) .ne. "type(a)  99") STOP 1

  call foo (u2, res)
  if (trim (res) .ne. "type(a) array 999 999 999") STOP 1

  if (SAME_TYPE_AS (obj1, u1) .neqv. .TRUE.) STOP 1

! Check allocate with an array SOURCE.
  allocate (u2(5), source = [(a(i), i = 1,5)])
  if (SAME_TYPE_AS (u1, a(2)) .neqv. .TRUE.) STOP 1
  call foo (u2, res)
  if (trim (res) .ne. "type(a) array   1   2   3   4   5") STOP 1

  deallocate (u2)

! Point to intrinsic targets.
  u1 => obj3
  call bar (u1, res)
  if (trim (res) .ne. "integer 999") STOP 1

  u2 => obj4
  call foo (u2, res)
  if (trim (res) .ne. "real array 1.0 2.0 3.0 4.0") STOP 1

  u2 => obj5
  call foo (u2, res)
  if (trim (res) .ne. "integer array  99 198 297") STOP 1

! Test allocate with source.
  allocate (u1, source = sun)
  call bar (u1, res)
  if (trim (res) .ne. "char( 8)sunshine") STOP 1
  deallocate (u1)

  allocate (u2(3), source = [7,8,9])
  call foo (u2, res)
  if (trim (res) .ne. "integer array   7   8   9") STOP 1

  deallocate (u2)

  if (EXTENDS_TYPE_OF (obj1, u2) .neqv. .TRUE.) STOP 1
  if (EXTENDS_TYPE_OF (u2, obj1) .neqv. .FALSE.) STOP 1

  allocate (u2(3), source = [5.0,6.0,7.0])
  call foo (u2, res)
  if (trim (res) .ne. "real array 5.0 6.0 7.0") STOP 1

  if (EXTENDS_TYPE_OF (obj1, u2) .neqv. .FALSE.) STOP 1
  if (EXTENDS_TYPE_OF (u2, obj1) .neqv. .FALSE.) STOP 1
  deallocate (u2)

! Check allocate with a MOLD tag.
  allocate (u2(3), mold = 8.0)
  call foo (u2, res)
  if (res(1:10) .ne. "real array") STOP 1
  deallocate (u2)

! Test passing an intrinsic type to a CLASS(*) formal.
  call bar(1, res)
  if (trim (res) .ne. "integer   1") STOP 1

  call bar(2.0, res)
  if (trim (res) .ne. "real4 2.0") STOP 1

  call bar(2d0, res)
  if (trim (res) .ne. "real8 2.0") STOP 1

  call bar(a(3), res)
  if (trim (res) .ne. "type(a)   3") STOP 1

  call bar(sun, res)
  if (trim (res) .ne. "char( 8)sunshine") STOP 1

  call bar (obj3, res)
  if (trim (res) .ne. "integer 999") STOP 1

  call foo([4,5], res)
  if (trim (res) .ne. "integer array   4   5") STOP 1

  call foo([6.0,7.0], res)
  if (trim (res) .ne. "real array 6.0 7.0") STOP 1

  call foo([a(8),a(9)], res)
  if (trim (res) .ne. "type(a) array   8   9") STOP 1

  call foo([sun, " & rain "], res)
  if (trim (res) .ne. "char( 8, 2)sunshine & rain") STOP 1

  call foo([sun//" never happens", " & rain always happens"], res)
  if (trim (res) .ne. "char(22, 2)sunshine never happens & rain always happens") STOP 1

  call foo (obj4, res)
  if (trim (res) .ne. "real array 1.0 2.0 3.0 4.0") STOP 1

  call foo (obj5, res)
  if (trim (res) .ne. "integer array  99 198 297") STOP 1

! Allocatable entities
  if (EXTENDS_TYPE_OF (obj1, u3) .neqv. .TRUE.) STOP 1
  if (EXTENDS_TYPE_OF (u3, obj1) .neqv. .FALSE.) STOP 1
  if (EXTENDS_TYPE_OF (obj1, u4) .neqv. .TRUE.) STOP 1
  if (EXTENDS_TYPE_OF (u4, obj1) .neqv. .FALSE.) STOP 1

  allocate (u3, source = 2.4)
  call bar (u3, res)
  if (trim (res) .ne. "real4 2.4") STOP 1

  allocate (u4(2), source = [a(88), a(99)])
  call foo (u4, res)
  if (trim (res) .ne. "type(a) array  88  99") STOP 1

  if (EXTENDS_TYPE_OF (obj1, u3) .neqv. .FALSE.) STOP 1
  if (EXTENDS_TYPE_OF (u3, obj1) .neqv. .FALSE.) STOP 1

  deallocate (u3)
  if (EXTENDS_TYPE_OF (obj1, u3) .neqv. .TRUE.) STOP 1
  if (EXTENDS_TYPE_OF (u3, obj1) .neqv. .FALSE.) STOP 1

  if (EXTENDS_TYPE_OF (obj1, u4) .neqv. .TRUE.) STOP 1
  if (EXTENDS_TYPE_OF (u4, obj1) .neqv. .TRUE.) STOP 1
  deallocate (u4)
  if (EXTENDS_TYPE_OF (obj1, u4) .neqv. .TRUE.) STOP 1
  if (EXTENDS_TYPE_OF (u4, obj1) .neqv. .FALSE.) STOP 1


! Check assumed rank calls
  call foobar (u3, 0, is_u3=.true.)
  call foobar (u4, 1, is_u3=.false.)
contains

  subroutine foobar (arg, ranki, is_u3)
    class(*) :: arg (..)
    integer :: ranki
    logical, value :: is_u3
    integer i
    i = rank (arg)
    if (i .ne. ranki) STOP 1
    if (is_u3) then
      if (EXTENDS_TYPE_OF (arg, obj1) .neqv. .FALSE.) STOP 1
    else
      ! arg == u4
      if (EXTENDS_TYPE_OF (arg, obj1) .neqv. .FALSE.) STOP 1
    end if
  !  if (.NOT. SAME_TYPE_AS (arg, u3)) STOP 1
  !  if (.NOT. SAME_TYPE_AS (arg, u4)) STOP 1
  end subroutine

END
