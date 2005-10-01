! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR20901 - Checks resolution of types in EQUIVALENCE statement when
! f95 standard is imposed.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
  type   :: numeric_type
    sequence
    integer  :: i
    real     :: x
    real*8   :: d
    complex  :: z
    logical  :: l
  end type numeric_type

  type (numeric_type) :: my_num, thy_num

  type   :: numeric_type2
    sequence
    integer  :: i
    real     :: x
    real*8   :: d
    complex  :: z
    logical  :: l
  end type numeric_type2

  type (numeric_type2) :: his_num

  type       :: char_type
    sequence
    character*4 :: ch
    character*4 :: cha (6)
  end type char_type

  type (char_type) ::  my_char

  type       :: mixed_type
    sequence
    integer*4 :: i(4)
    character*4 :: cha (6)
  end type mixed_type

  type (mixed_type) ::  my_mixed, thy_mixed

  character(len=4) :: ch
  integer :: num
  integer*8 :: non_def
  complex*16 :: my_z, thy_z

! Permitted: character with character sequence
!            numeric with numeric sequence
!            numeric sequence with numeric sequence
!            non-default of same type
!            mixed sequences of same type
  equivalence (ch, my_char)
  equivalence (num, my_num)
  equivalence (my_num, his_num, thy_num)
  equivalence (my_z, thy_z)
  equivalence (my_mixed, thy_mixed)

! Not permitted by the standard - OK with -std=gnu
  equivalence (my_mixed, my_num) ! { dg-error "with mixed components in EQUIVALENCE" }
  equivalence (my_z, num) ! { dg-error "Non-default type object or sequence" }
  equivalence (my_char, my_num) ! { dg-error "in default CHARACTER EQUIVALENCE" }
  equivalence (ch, my_num) ! { dg-error "in default CHARACTER EQUIVALENCE" }
  equivalence (my_num, ch) ! { dg-error "in default NUMERIC EQUIVALENCE" }
  equivalence (num, my_char) ! { dg-error "in default NUMERIC EQUIVALENCE" }
  equivalence (my_char, num) ! { dg-error "in default CHARACTER EQUIVALENCE" }
  equivalence (non_def, ch) ! { dg-error "Non-default type object or sequence" }
  equivalence (my_z, ch) ! { dg-error "Non-default type object or sequence" }
  equivalence (my_z, num) ! { dg-error "Non-default type object or sequence" }
 END
