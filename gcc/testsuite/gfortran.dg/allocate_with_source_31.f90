! { dg-do run }
! { dg-additional-options "-std=gnu -fcheck=no-bounds" }
!
! PR fortran/113793
!
! Test extension for ALLOCATE with SOURCE= or MOLD= that strings
! are truncated or padded and no memory corruption occurs

program p
  implicit none
  call test_pad   (8, "12345")
  call test_trunc (6, "123456789")
contains
  subroutine test_pad (n, s)
    integer,      intent(in) :: n
    character(*), intent(in) :: s
    character(len=n), allocatable :: a(:), b(:,:)
    if (len (s) >= n) stop 111
    ALLOCATE (a(100),source=s)
    ALLOCATE (b(5,6),source=s)
!   print *, ">", a(42), "<"
!   print *, ">", b(3,4), "<"
    if (a(42)  /= s) stop 1
    if (b(3,4) /= s) stop 2
  end
  subroutine test_trunc (n, s)
    integer,      intent(in) :: n
    character(*), intent(in) :: s
    character(len=n), allocatable :: a(:), b(:,:)
    if (len (s) <= n) stop 222
    ALLOCATE (a(100),source=s)
    ALLOCATE (b(5,6),source=s)
!   print *, ">", a(42), "<"
!   print *, ">", b(3,4), "<"
    if (a(42)  /= s(1:n)) stop 3
    if (b(3,4) /= s(1:n)) stop 4
  end
end
