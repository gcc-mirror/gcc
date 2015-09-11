! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR66079. The original problem was with the first
! allocate statement. The rest of this testcase fixes problems found
! whilst working on it!
!
! Reported by Damian Rouson  <damian@sourceryinstitute.org>
!
  type subdata
    integer, allocatable :: b
  endtype
!  block
    call newRealVec
!  end block
contains
  subroutine newRealVec
    type(subdata), allocatable :: d, e, f
    character(:), allocatable :: g, h, i
    character(8), allocatable :: j
    allocate(d,source=subdata(1)) ! memory was lost, now OK
    allocate(e,source=d) ! OK
    allocate(f,source=create (99)) ! memory was lost, now OK
    if (d%b .ne. 1) call abort
    if (e%b .ne. 1) call abort
    if (f%b .ne. 99) call abort
    allocate (g, source = greeting1("good day"))
    if (g .ne. "good day") call abort
    allocate (h, source = greeting2("hello"))
    if (h .ne. "hello") call abort
    allocate (i, source = greeting3("hiya!"))
    if (i .ne. "hiya!") call abort
    call greeting4 (j, "Goodbye ") ! Test that dummy arguments are OK
    if (j .ne. "Goodbye ") call abort
  end subroutine

  function create (arg) result(res)
    integer :: arg
    type(subdata), allocatable :: res, res1
    allocate(res, res1, source = subdata(arg))
  end function

  function greeting1 (arg) result(res) ! memory was lost, now OK
    character(*) :: arg
    Character(:), allocatable :: res
    allocate(res, source = arg)
  end function

  function greeting2 (arg) result(res)
    character(5) :: arg
    Character(:), allocatable :: res
    allocate(res, source = arg)
  end function

  function greeting3 (arg) result(res)
    character(5) :: arg
    Character(5), allocatable :: res, res1
    allocate(res, res1, source = arg) ! Caused an ICE
    if (res1 .ne. res) call abort
  end function

  subroutine greeting4 (res, arg)
    character(8), intent(in) :: arg
    Character(8), allocatable, intent(out) :: res
    allocate(res, source = arg) ! Caused an ICE
  end subroutine
end
! { dg-final { scan-tree-dump-times "builtin_malloc" 20 "original" } }
! { dg-final { scan-tree-dump-times "builtin_free" 21 "original" } }

