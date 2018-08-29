! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Runtime tests for rules governing dot ('.') as a member accessor, including
! voodoo with aliased user-defined vs. intrinsic operators and nested members.
! See gcc/fortran/match.c (gfc_match_member_sep).
!

module dec_structure_10
  ! Operator overload tests with .ne. and constant member
  structure /s1/
    integer i
    integer ne
    logical b
  end structure

  ! Operator overload tests with .eq., .test. and nested members
  structure /s2/
    record /s1/ eq
    record /s1/ test
    record /s1/ and
    integer i
  end structure

  ! Deep nested access tests
  structure /s3/
    record /s2/ r2
  end structure
  structure /s4/
    record /s3/ r3
  end structure
  structure /s5/
    record /s4/ r4
  end structure
  structure /s6/
    record /s5/ r5
  end structure
  structure /s7/
    record /s6/ r6
  end structure

  ! Operator overloads to mess with nested member accesses
  interface operator (.ne.)
    module procedure ne_func
  end interface operator (.ne.)
  interface operator (.eq.)
    module procedure eq_func
  end interface operator (.eq.)
  interface operator (.test.)
    module procedure tstfunc
  end interface operator (.test.)
  contains
  ! ne_func will be called on (x) .ne. (y)
  function ne_func (r, i)
    integer, intent(in) :: i
    type(s1), intent(in) :: r
    integer ne_func
    ne_func = r%i + i
  end function
  ! eq_func will be called on (x) .eq. (y)
  function eq_func (r, i)
    integer, intent(in) :: i
    type(s2), intent(in) :: r
    integer eq_func
    eq_func = r%eq%i + i
  end function eq_func
  ! tstfunc will be called on (x) .test. (y)
  function tstfunc (r, i)
    integer, intent(in) :: i
    type(s2), intent(in) :: r
    integer tstfunc
    tstfunc = r%i + i
  end function tstfunc
end module

use dec_structure_10

record /s1/ r
record /s2/ struct
record /s7/ r7
integer i, j
logical l
struct%eq%i = 5
i = -5

! Nested access: struct has a member and which has a member b
l =  struct .and. b   ! struct%and%b
l =  struct .and. b .or. .false. ! (struct%and%b) .or. (.false.)

! Intrinsic op: r has no member 'ne'
j =  r .ne. i         ! <intrinsic> ne(r, i)
j = (r) .ne. i        ! <intrinsic> ne(r, i)

! Intrinsic op; r has a member 'ne' but it is not a record
j =  r .ne. i         ! <intrinsic> ne(r, i)
j = (r) .ne. i        ! <intrinsic> ne(r, i)

! Nested access: struct has a member eq which has a member i
j =  struct .eq. i    ! struct%eq%i
if ( j .ne. struct%eq%i ) STOP 1

! User op: struct is compared to i with eq_func
j = (struct) .eq. i   ! eq_func(struct, i) -> struct%eq%i + i
if ( j .ne. struct%eq%i + i ) STOP 2

! User op: struct has a member test which has a member i, but test is a uop
j =  struct .test. i  ! tstfunc(struct, i) -> struct%i + i
if ( j .ne. struct%i + i ) STOP 3

! User op: struct is compared to i with eq_func
j = (struct) .test. i ! tstfunc(struct, i) -> struct%i + i
if ( j .ne. struct%i + i ) STOP 4

! Deep nested access tests
r7.r6.r5.r4.r3.r2.i = 1337
j = r7.r6.r5.r4.r3.r2.i
if ( j .ne. 1337 ) STOP 5

end
