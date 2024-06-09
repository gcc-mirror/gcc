! { dg-do run }
! PR fortran/113377
!
! Test passing of missing optional scalar dummies of intrinsic type

module m_int
  implicit none
contains
  subroutine test_int ()
    integer :: k = 1
    call one     (k)
    call one_val (k)
    call one_all (k)
    call one_ptr (k)
  end

  subroutine one (i, j)
    integer, intent(in)           :: i
    integer             ,optional :: j
    integer, allocatable :: aa
    integer, pointer     :: pp => NULL()
    if (present (j)) error stop "j is present"
    call two     (i, j)
    call two_val (i, j)
    call two     (i, aa)
    call two     (i, pp)
    call two_val (i, aa)
    call two_val (i, pp)
  end

  subroutine one_val (i, j)
    integer, intent(in)           :: i
    integer, value,      optional :: j
    if (present (j)) error stop "j is present"
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_all (i, j)
    integer, intent(in)           :: i
    integer, allocatable,optional :: j
    if (present (j)) error stop "j is present"
!   call two     (i, j)  ! invalid per F2018:15.5.2.12, par. 3, clause 8
!   call two_val (i, j)  ! dto.
    call two_all (i, j)
  end

  subroutine one_ptr (i, j)
    integer, intent(in)           :: i
    integer, pointer    ,optional :: j
    if (present (j)) error stop "j is present"
!   call two     (i, j)  ! invalid per F2018:15.5.2.12, par. 3, clause 7
!   call two_val (i, j)  ! dto.
    call two_ptr (i, j)
  end

  subroutine two (i, j)
    integer, intent(in)           :: i
    integer, intent(in), optional :: j
    if (present (j)) error stop 11
  end

  subroutine two_val (i, j)
    integer, intent(in)           :: i
    integer, value,      optional :: j
    if (present (j)) error stop 12
  end

  subroutine two_all (i, j)
    integer, intent(in)           :: i
    integer, allocatable,optional :: j
    if (present (j)) error stop 13
  end

  subroutine two_ptr (i, j)
    integer, intent(in)           :: i
    integer, pointer,    optional :: j
    if (present (j)) error stop 14
  end
end

module m_char
  implicit none
contains
  subroutine test_char ()
    character :: k = "#"
    call one     (k)
    call one_val (k)
    call one_all (k)
    call one_ptr (k)
  end

  subroutine one (i, j)
    character, intent(in)           :: i
    character             ,optional :: j
    character, allocatable :: aa
    character, pointer     :: pp => NULL()
    if (present (j)) error stop "j is present"
    call two     (i, j)
    call two_val (i, j)
    call two     (i, aa)
    call two     (i, pp)
  end

  subroutine one_val (i, j)
    character, intent(in)           :: i
    character, value,      optional :: j
    if (present (j)) error stop "j is present"
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_all (i, j)
    character, intent(in)           :: i
    character, allocatable,optional :: j
    if (present (j)) error stop "j is present"
!   call two     (i, j)  ! invalid per F2018:15.5.2.12, par. 3, clause 8
!   call two_val (i, j)  ! dto.
    call two_all (i, j)
  end

  subroutine one_ptr (i, j)
    character, intent(in)           :: i
    character, pointer    ,optional :: j
    if (present (j)) error stop "j is present"
!   call two     (i, j)  ! invalid per F2018:15.5.2.12, par. 3, clause 7
!   call two_val (i, j)  ! dto.
    call two_ptr (i, j)
  end

  subroutine two (i, j)
    character, intent(in)           :: i
    character, intent(in), optional :: j
    if (present (j)) error stop 21
  end

  subroutine two_val (i, j)
    character, intent(in)           :: i
    character, value,      optional :: j
    if (present (j)) error stop 22
  end

  subroutine two_all (i, j)
    character, intent(in)           :: i
    character, allocatable,optional :: j
    if (present (j)) error stop 23
  end

  subroutine two_ptr (i, j)
    character, intent(in)           :: i
    character, pointer,    optional :: j
    if (present (j)) error stop 24
  end
end

module m_char4
  implicit none
contains
  subroutine test_char4 ()
    character(kind=4) :: k = 4_"#"
    call one     (k)
    call one_val (k)
    call one_all (k)
    call one_ptr (k)
  end

  subroutine one (i, j)
    character(kind=4), intent(in)           :: i
    character(kind=4)             ,optional :: j
    character(kind=4), allocatable :: aa
    character(kind=4), pointer     :: pp => NULL()
    if (present (j)) error stop "j is present"
    call two     (i, j)
    call two_val (i, j)
    call two     (i, aa)
    call two     (i, pp)
  end

  subroutine one_val (i, j)
    character(kind=4), intent(in)           :: i
    character(kind=4), value,      optional :: j
    if (present (j)) error stop "j is present"
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_all (i, j)
    character(kind=4), intent(in)           :: i
    character(kind=4), allocatable,optional :: j
    if (present (j)) error stop "j is present"
!   call two     (i, j)  ! invalid per F2018:15.5.2.12, par. 3, clause 8
!   call two_val (i, j)  ! dto.
    call two_all (i, j)
  end

  subroutine one_ptr (i, j)
    character(kind=4), intent(in)           :: i
    character(kind=4), pointer    ,optional :: j
    if (present (j)) error stop "j is present"
!   call two     (i, j)  ! invalid per F2018:15.5.2.12, par. 3, clause 7
!   call two_val (i, j)  ! dto.
    call two_ptr (i, j)
  end

  subroutine two (i, j)
    character(kind=4), intent(in)           :: i
    character(kind=4), intent(in), optional :: j
    if (present (j)) error stop 31
  end

  subroutine two_val (i, j)
    character(kind=4), intent(in)           :: i
    character(kind=4), value,      optional :: j
    if (present (j)) error stop 32
  end

  subroutine two_all (i, j)
    character(kind=4), intent(in)           :: i
    character(kind=4), allocatable,optional :: j
    if (present (j)) error stop 33
  end

  subroutine two_ptr (i, j)
    character(kind=4), intent(in)           :: i
    character(kind=4), pointer,    optional :: j
    if (present (j)) error stop 34
  end
end

module m_complex
  implicit none
contains
  subroutine test_complex ()
    complex :: k = 3.
    call one     (k)
    call one_val (k)
    call one_all (k)
    call one_ptr (k)
  end

  subroutine one (i, j)
    complex, intent(in)           :: i
    complex             ,optional :: j
    complex, allocatable :: aa
    complex, pointer     :: pp => NULL()
    if (present (j)) error stop "j is present"
    call two     (i, j)
    call two_val (i, j)
    call two     (i, aa)
    call two     (i, pp)
  end

  subroutine one_val (i, j)
    complex, intent(in)           :: i
    complex, value,      optional :: j
    if (present (j)) error stop "j is present"
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_all (i, j)
    complex, intent(in)           :: i
    complex, allocatable,optional :: j
    if (present (j)) error stop "j is present"
!   call two     (i, j)  ! invalid per F2018:15.5.2.12, par. 3, clause 8
!   call two_val (i, j)  ! dto.
    call two_all (i, j)
  end

  subroutine one_ptr (i, j)
    complex, intent(in)           :: i
    complex, pointer    ,optional :: j
    if (present (j)) error stop "j is present"
!   call two     (i, j)  ! invalid per F2018:15.5.2.12, par. 3, clause 7
!   call two_val (i, j)  ! dto.
    call two_ptr (i, j)
  end

  subroutine two (i, j)
    complex, intent(in)           :: i
    complex, intent(in), optional :: j
    if (present (j)) error stop 41
  end

  subroutine two_val (i, j)
    complex, intent(in)           :: i
    complex, value,      optional :: j
    if (present (j)) error stop 42
  end

  subroutine two_all (i, j)
    complex, intent(in)           :: i
    complex, allocatable,optional :: j
    if (present (j)) error stop 43
  end

  subroutine two_ptr (i, j)
    complex, intent(in)           :: i
    complex, pointer,    optional :: j
    if (present (j)) error stop 44
  end
end

module m_mm
  ! Test suggested by Mikael Morin
  implicit none
  type :: t
    integer, allocatable :: c
    integer, pointer     :: p => NULL()
  end type
contains
  subroutine test_mm ()
    call s1 (t())
  end

  subroutine s1 (a)
    type(t) :: a
    call s2 (a% c)
    call s2 (a% p)
  end
  
  subroutine s2 (a)
    integer, value, optional :: a
    if (present(a)) stop 1
  end
end

program p
  use m_int
  use m_char
  use m_char4
  use m_complex
  use m_mm
  implicit none
  call test_int ()
  call test_char ()
  call test_char4 ()
  call test_complex ()
  call test_mm ()
end
