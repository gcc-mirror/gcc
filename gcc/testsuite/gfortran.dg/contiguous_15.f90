! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/97592 - fix argument passing to CONTIGUOUS,TARGET dummy
!
! { dg-final { scan-tree-dump-times "_gfortran_internal_pack \\(&b_2d" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_internal_pack \\(&p1" 3 "original" } }
!
! N.B.: there is no reliable count of _gfortran_internal_pack on temporaries parm.*

program pr97592
  implicit none
  integer :: i, k
  integer, target              :: a(10)
  integer, pointer             :: p1(:), p2(:), tgt(:), expect(:)
  integer, pointer, contiguous :: cp(:)
  integer, allocatable, target :: b(:)

  !----------------------
  ! Code from original PR
  !----------------------
  call RemappingTest ()

  !---------------------
  ! Additional 1-d tests
  !---------------------
  a = [(i, i=1,size(a))]
  b = a

  ! Set p1 to an actually contiguous pointer
  p1(13:) => a(3::2)
  print *, lbound (p1), ubound (p1), is_contiguous (p1)

  ! non-contiguous pointer actual argument
  expect => p1
  call chk_cont (p1)

  expect => p1
  call chk_tgt_cont (p1)

  expect => p1
  call chk_ptr  (p1, p2)
  if (any (p2 /= p1)) stop 1

  expect => p1
  call chk_tgt  (p1, p2)
  if (any (p2 /= p1)) stop 2

  ! non-contiguous target actual argument
  expect => b(3::2)
  call chk_tgt_cont (b(3::2))

  expect => b(3::2)
  call chk_tgt (b(3::2), p2)
  if (any (p2 /= p1)) stop 3

  expect => b(3::2)
  call chk_ptr  (b(3::2), p2)
  if (any (p2 /= p1)) stop 4

  ! Set p1 to an actually contiguous pointer
  cp(17:) => a(3:9:1)
  p1 => cp
  print *, lbound (cp), ubound (cp), is_contiguous (cp)
  print *, lbound (p1), ubound (p1), is_contiguous (p1)

  expect => p1
  call chk_tgt  (p1, p2)
  if (any (p2 /= cp)) stop 31

  expect => cp
  call chk_tgt  (cp, p2)
  if (any (p2 /= cp)) stop 32

  expect => cp
  call chk_tgt_cont (cp, p2)
  if (any (p2 /= cp)) stop 33

  expect => cp
  call chk_tgt_expl (cp, p2, size (cp))
  if (any (p2 /= cp)) stop 34

  ! See F2018:15.5.2.4 and F2018:C.10.4
  expect => p1
  call chk_tgt_cont (p1, p2)
! print *, p2
  if (any (p2 /= cp)) stop 35

  expect => p1
  call chk_tgt_expl (p1, p2, size (p1))
  if (any (p2 /= cp)) stop 36

  expect => cp
  call chk_ptr_cont (cp, p2)
  if (any (p2 /= cp)) stop 37

  ! Pass array section which is actually contigous
  k = 1
  expect => cp(::k)
  call chk_ptr (cp(::k), p2)
  if (any (p2 /= cp(::k))) stop 38

  expect => p1(::k)
  call chk_tgt_cont (p1(::k), p2)
  if (any (p2 /= p1(::k))) stop 39

  expect => p1(::k)
  call chk_tgt (p1(::k), p2)
  if (any (p2 /= p1(::k))) stop 40

  expect => p1(::k)
  call chk_tgt_expl (p1(::k), p2, size (p1(::k)))
  if (any (p2 /= p1(::k))) stop 41

  expect => b(3::k)
  call chk_tgt_cont (b(3::k), p2)
  if (any (p2 /= b(3::k))) stop 42

  expect => b(3::k)
  call chk_tgt (b(3::k), p2)
  if (any (p2 /= b(3::k))) stop 43

  expect => b(3::k)
  call chk_tgt_expl (b(3::k), p2, size (b(3::k)))
  if (any (p2 /= b(3::k))) stop 44

  if (any (a /= [(i, i=1,size(a))])) stop 66
  if (any (a /= b))                  stop 77
  deallocate (b)

contains
  ! Contiguous pointer dummy
  subroutine chk_ptr_cont (x, y)
    integer, contiguous, pointer, intent(in) :: x(:)
    integer, pointer,    optional            :: y(:)
    print *, lbound (x), ubound (x)
    if (present (y)) y => x(:)
    if (associated (expect)) then
       if (size (x) /= size (expect)) stop 10
       if (any (x /= expect))         stop 11
       if (lbound(expect,1) /= 1   .and.   &
           lbound(expect,1) /= lbound (x,1)) stop 20
    end if
  end

  ! Pointer dummy
  subroutine chk_ptr (x, y)
    integer, pointer, intent(in) :: x(:)
    integer, pointer, optional   :: y(:)
    print *, lbound (x), ubound (x)
    if (present (y)) y => x(:)
    if (associated (expect)) then
       if (size (x) /= size (expect)) stop 12
       if (any (x /= expect))         stop 13
       if (lbound(expect,1) /= 1   .and.   &
           lbound(expect,1) /= lbound (x,1)) stop 22
    end if
  end

  ! Dummy with target attribute
  subroutine chk_tgt_cont (x, y)
    integer, contiguous, target,  intent(in) :: x(:)
    integer, pointer,    optional            :: y(:)
    if (present (y)) y => x(:)
    if (associated (expect)) then
       if (size (x) /= size (expect)) stop 14
       if (any (x /= expect))         stop 15
    end if
  end

  subroutine chk_tgt (x, y)
    integer, target,  intent(in) :: x(:)
    integer, pointer, optional   :: y(:)
    if (present (y)) y => x(:)
    if (associated (expect)) then
       if (size (x) /= size (expect)) stop 16
       if (any (x /= expect))         stop 17
    end if
  end

  ! Explicit-shape dummy with target attribute
  subroutine chk_tgt_expl (x, y, n)
    integer,         intent(in) :: n
    integer, target, intent(in) :: x(n)
    integer, pointer, optional  :: y(:)
    if (present (y)) y => x(:)
    if (associated (expect)) then
       if (size (x) /= size (expect)) stop 18
       if (any (x /= expect))         stop 19
    end if
  end

  ! Dummy without pointer or target attribute
  subroutine chk_cont (x)
    integer, contiguous, intent(in) :: x(:)
    if (associated (expect)) then
       if (size (x) /= size (expect)) stop 23
       if (any (x /= expect))         stop 24
    end if
  end

  !------------------------------------------------------------------------

  subroutine RemappingTest ()
    real, pointer      :: B_2D(:,:)
    real, pointer      :: B_3D(:,:,:) => NULL()
    integer, parameter :: n1=4, n2=4, n3=3
    !-- Prepare B_2D
    allocate (B_2D(n1*n2, n3))
    B_2D = - huge (1.0)
    if (.not. is_contiguous (B_2D)) stop 101
    !-- Point B_3D to Storage
    call SetPointer (B_2D, n1, n2, n3, B_3D)
    !print *,"is_contiguous (B_3D) =", is_contiguous (B_3D)
    if (.not. is_contiguous (B_3D)) stop 102
    !-- Set B_3D
    B_3D = 2.0
    !-- See if the result is reflected in Storage
    if (any (B_2D /= 2.0)) then
       print *, "B_2D = ", B_2D  !-- expect 2.0 for all elements
       stop 103
    end if
    print *,"RemappingTest passed"
  end

  subroutine SetPointer (C_2D, n1, n2, n3, C_3D)
    integer,       intent(in) :: n1, n2, n3
    real, target,  contiguous :: C_2D(:,:)
    real, pointer             :: C_3D(:,:,:)
    intent(in)                :: C_2D
    C_3D(1:n1,1:n2,1:n3) => C_2D
  end

end
