! { dg-do run }
! PR fortran/113377
!
! Test passing of missing optional arguments of intrinsic type
! to scalar dummies of elemental subroutines

module m_char
  implicit none
contains
  subroutine test_char ()
    character    :: k(7) = "#"
    character(4) :: c(7) = "*"
    call one     (k)
    call one_val (k)
    call one_ij  (k)
    call one_jj  (k)
    call one_j4  (k)
    call three     (c)
    call three_val (c)
    call three_ij  (c)
    call three_jj  (c)
    call three_j4  (c)
  end subroutine test_char

  subroutine one (i, j)
    character, intent(in)           :: i(7)
    character, intent(in), optional :: j
    character, allocatable :: aa
    character, pointer     :: pp => NULL()
    if (present (j)) stop 1
    call two     (i, j)
    call two_val (i, j)
    call two     (i, aa)
    call two     (i, pp)
    call two_val (i, aa)
    call two_val (i, pp)
  end

  subroutine one_val (i, j)
    character, intent(in)           :: i(7)
    character, value,      optional :: j
    if (present (j)) stop 2
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_ij (i, j)
    character, intent(in)           :: i(7)
    character, intent(in), optional :: j(7)
    if (present (j)) stop 3
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_jj (i, j)
    character, intent(in)           :: i(7)
    character, intent(in), optional :: j(:)
    if (present (j)) stop 4
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_j4 (i, j)
    character, intent(in)           :: i(:)
    character, intent(in), optional :: j(7)
    if (present (j)) stop 5
    call two     (i, j)
    call two_val (i, j)
  end

  elemental subroutine two (i, j)
    character, intent(in)           :: i
    character, intent(in), optional :: j
    if (present (j)) error stop 11
  end

  elemental subroutine two_val (i, j)
    character, intent(in)           :: i
    character, value,      optional :: j
    if (present (j)) error stop 12
  end

  subroutine three (i, j)
    character(4), intent(in)           :: i(7)
    character(4), intent(in), optional :: j
    character(4), allocatable :: aa
    character(4), pointer     :: pp => NULL()
    if (present (j)) stop 6
    call four     (i, j)
    call four_val (i, j)
    call four     (i, aa)
    call four     (i, pp)
    call four_val (i, aa)
    call four_val (i, pp)
  end

  subroutine three_val (i, j)
    character(4), intent(in)           :: i(7)
    character(4), value,      optional :: j
    if (present (j)) stop 7
    call four     (i, j)
    call four_val (i, j)
  end

  subroutine three_ij (i, j)
    character(4), intent(in)           :: i(7)
    character(4), intent(in), optional :: j(7)
    if (present (j)) stop 8
    call four     (i, j)
    call four_val (i, j)
  end

  subroutine three_jj (i, j)
    character(4), intent(in)           :: i(7)
    character(4), intent(in), optional :: j(:)
    if (present (j)) stop 9
    call four     (i, j)
    call four_val (i, j)
  end

  subroutine three_j4 (i, j)
    character(4), intent(in)           :: i(:)
    character(4), intent(in), optional :: j(7)
    if (present (j)) stop 10
    call four     (i, j)
    call four_val (i, j)
  end

  elemental subroutine four (i, j)
    character(4), intent(in)           :: i
    character(4), intent(in), optional :: j
    if (present (j)) error stop 13
  end

  elemental subroutine four_val (i, j)
    character(4), intent(in)           :: i
    character(4), value,      optional :: j
    if (present (j)) error stop 14
  end
end

module m_int
  implicit none
contains
  subroutine test_int ()
    integer :: k(4) = 1
    call one     (k)
    call one_val (k)
    call one_ij  (k)
    call one_jj  (k)
    call one_j4  (k)
  end

  subroutine one (i, j)
    integer, intent(in)           :: i(4)
    integer, intent(in), optional :: j
    integer, allocatable :: aa
    integer, pointer     :: pp => NULL()
    if (present (j)) stop 21
    call two     (i, j)
    call two_val (i, j)
    call two     (i, aa)
    call two     (i, pp)
    call two_val (i, aa)
    call two_val (i, pp)
  end

  subroutine one_val (i, j)
    integer, intent(in)           :: i(4)
    integer, value,      optional :: j
    if (present (j)) stop 22
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_ij (i, j)
    integer, intent(in)           :: i(4)
    integer, intent(in), optional :: j(4)
    if (present (j)) stop 23
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_jj (i, j)
    integer, intent(in)           :: i(4)
    integer, intent(in), optional :: j(:)
    if (present (j)) stop 24
    call two     (i, j)
    call two_val (i, j)
  end

  subroutine one_j4 (i, j)
    integer, intent(in)           :: i(:)
    integer, intent(in), optional :: j(4)
    if (present (j)) stop 25
    call two     (i, j)
    call two_val (i, j)
  end

  elemental subroutine two (i, j)
    integer, intent(in)           :: i
    integer, intent(in), optional :: j
    if (present (j)) error stop 31
  end

  elemental subroutine two_val (i, j)
    integer, intent(in)           :: i
    integer, value,      optional :: j
    if (present (j)) error stop 32
  end
end

program p
  use m_int
  use m_char
  implicit none
  call test_int ()
  call test_char ()
end
