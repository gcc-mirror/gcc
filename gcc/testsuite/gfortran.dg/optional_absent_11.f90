! { dg-do run }
! PR fortran/113377
!
! Test that a NULL actual argument to an optional dummy is not present
! (see also F2018:15.5.2.12 on argument presence)

program test_null_actual_is_absent
  implicit none
  integer   :: k(4) = 1
  character :: c(4) = "#"
  call one   (k)
  call three (c)
contains
  subroutine one (i)
    integer, intent(in)  :: i(4)
    integer              :: kk = 2
    integer, allocatable :: aa
    integer, pointer     :: pp => NULL()
    print *, "Scalar integer"
    call two     (kk, aa)
    call two     (kk, pp)
    call two     (kk, NULL())
    call two     (kk, NULL(aa))
    call two     (kk, NULL(pp))
    print *, "Elemental integer"
    call two     (i,  aa)
    call two     (i,  pp)
    call two     (i,  NULL())
    call two     (i,  NULL(aa))
    call two     (i,  NULL(pp))
    print *, "Scalar integer; value"
    call two_val (kk, aa)
    call two_val (kk, pp)
    call two_val (kk, NULL())
    call two_val (kk, NULL(aa))
    call two_val (kk, NULL(pp))
    print *, "Elemental integer; value"
    call two_val (i,  aa)
    call two_val (i,  pp)
    call two_val (i,  NULL())
    call two_val (i,  NULL(aa))
    call two_val (i,  NULL(pp))
  end

  elemental subroutine two (i, j)
    integer, intent(in)           :: i
    integer, intent(in), optional :: j
    if (present (j)) error stop 11
  end

  elemental subroutine two_val (i, j)
    integer, intent(in)           :: i
    integer, value,      optional :: j
    if (present (j)) error stop 12
  end

  subroutine three (y)
    character, intent(in)  :: y(4)
    character              :: zz = "*"
    character, allocatable :: aa
    character, pointer     :: pp => NULL()
    print *, "Scalar character"
    call four     (zz, aa)
    call four     (zz, pp)
    call four     (zz, NULL())
    call four     (zz, NULL(aa))
    call four     (zz, NULL(pp))
    print *, "Elemental character"
    call four     (y,  aa)
    call four     (y,  pp)
    call four     (y,  NULL())
    call four     (y,  NULL(aa))
    call four     (y,  NULL(pp))
    print *, "Scalar character; value"
    call four_val (zz, aa)
    call four_val (zz, pp)
    call four_val (zz, NULL())
    call four_val (zz, NULL(aa))
    call four_val (zz, NULL(pp))
    print *, "Elemental character; value"
    call four_val (y,  aa)
    call four_val (y,  pp)
    call four_val (y,  NULL())
    call four_val (y,  NULL(aa))
    call four_val (y,  NULL(pp))
  end

  elemental subroutine four (i, j)
    character, intent(in)           :: i
    character, intent(in), optional :: j
    if (present (j)) error stop 21
  end

  elemental subroutine four_val (i, j)
    character, intent(in)           :: i
    character, value,      optional :: j
    if (present (j)) error stop 22
  end
end
