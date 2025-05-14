! { dg-do compile }
!
! Test argument compliance for the F2018 intrinsic REDUCE
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  class (*), allocatable :: cstar (:)
  integer, allocatable :: i(:,:,:)
  integer :: n(2,2)
  Logical :: l1(4), l2(2,3), l3(2,2)
  type :: string_t
    character(:), allocatable :: chr(:)
  end type
  type(string_t) :: str

! The ARRAY argument at (1) of REDUCE shall not be polymorphic
  print *, reduce (cstar, add) ! { dg-error "shall not be polymorphic" }

! OPERATION argument at %L must be a PURE function
  print *, reduce (i, iadd) ! { dg-error "must be a PURE function" }
  print *, reduce (i, foo) ! { dg-error "must be a PURE function" }

! The function passed as OPERATION at (1) shall have scalar nonallocatable
! nonpointer arguments and return a nonallocatable nonpointer scalar
  print *, reduce (i, vadd) ! { dg-error "return a nonallocatable nonpointer scalar" }

! The function passed as OPERATION at (1) shall have two arguments
  print *, reduce (i, add_1a) ! { dg-error "shall have two arguments" }
  print *, reduce (i, add_3a) ! { dg-error "shall have two arguments" }

!The ARRAY argument at (1) has type INTEGER(4) but the function passed as OPERATION at
! (2) returns REAL(4)
  print *, reduce (i, add_r) ! { dg-error "returns REAL" }

! The function passed as OPERATION at (1) shall have scalar nonallocatable nonpointer
! arguments and return a nonallocatable nonpointer scalar
  print *, reduce (i, add_a) ! { dg-error "return a nonallocatable nonpointer scalar" }

! The function passed as OPERATION at (1) shall have scalar nonallocatable nonpointer arguments and
! return a nonallocatable nonpointer scalar
  print *, reduce (i, add_array) ! { dg-error "scalar nonallocatable nonpointer arguments" }

! The function passed as OPERATION at (1) shall not have the OPTIONAL attribute for either of the arguments
  print *, reduce (i, add_optional) ! { dg-error "shall not have the OPTIONAL attribute" }

! The function passed as OPERATION at (1) shall have the VALUE attribute either for none or both arguments
  print *, reduce (i, add_one_value) ! { dg-error "VALUE attribute either for none or both arguments" }

! The character length of the ARRAY argument at (1) and of the arguments of the OPERATION at (2)
! shall be the same
  print *, reduce ([character(4) :: 'abcd','efgh'], char_one) ! { dg-error "The character length of the ARRAY" }

! The character length of the ARRAY argument at (1) and of the function result of the OPERATION
! at (2) shall be the same
  print *, reduce ([character(4) :: 'abcd','efgh'], char_two) ! { dg-error "function result of the OPERATION" }

! The character length of the ARRAY argument at (1) and of the arguments of the OPERATION at
! (2) shall be the same
  print *, reduce ([character(4) :: 'abcd','efgh'], char_three) ! { dg-error "arguments of the OPERATION" }

! The character length of the ARRAY argument at (1) and of the arguments of the OPERATION at (2)
! shall be the same
  str = reduce ([character(4) :: 'abcd','efgh'], char_one) ! { dg-error "character length of the ARRAY" }

! The DIM argument at (1), if present, must be an integer scalar
  print *, reduce (i, add, dim = 2.0) ! { dg-error "must be an integer scalar" }

! The DIM argument at (1), if present, must be an integer scalar
  print *, reduce (i, add, dim = [2]) ! { dg-error "must be an integer scalar" }

! The MASK argument at (1), if present, must be a logical array with the same rank as ARRAY
  print *, reduce (n, add, mask = l1) ! { dg-error "same rank as ARRAY" }
  print *, reduce (n, add, mask = n) ! { dg-error "must be a logical array" }

! Different shape for arguments 'ARRAY' and 'MASK' for intrinsic REDUCE at (1) on
! dimension 2 (2 and 3)
  print *, reduce (n, add, mask = l2) ! { dg-error "Different shape" }

! The IDENTITY argument at (1), if present, must be a scalar with the same type as ARRAY
  print *, reduce (n, add, mask = l3, identity = 1.0) ! { dg-error "same type as ARRAY" }
  print *, reduce (n, add, mask = l3, identity = [1]) ! { dg-error "must be a scalar" }

! MASK present at (1) without IDENTITY
  print *, reduce (n, add, mask = l3) ! { dg-warning "without IDENTITY" }

contains
  pure function add(i,j) result(sum_ij)
    integer, intent(in) :: i, j
    integer             :: sum_ij
    sum_ij = i + j
  end function add
  function iadd(i,j) result(sum_ij)
    integer, intent(in) :: i, j
    integer             :: sum_ij
    sum_ij = i + j
  end function iadd
  pure function vadd(i,j) result(sum_ij)
    integer, intent(in) :: i, j
    integer             :: sum_ij(6)
    sum_ij = i + j
  end function vadd
  pure function add_1a(i) result(sum_ij)
    integer, intent(in) :: i
    integer             :: sum_ij
    sum_ij = 0
  end function add_1a
  pure function add_3a(i) result(sum_ij)
    integer, intent(in) :: i
    integer             :: sum_ij
    sum_ij = 0
  end function add_3a
  pure function add_r(i, j) result(sum_ij)
    integer, intent(in) :: i, j
    real             :: sum_ij
    sum_ij = 0.0
  end function add_r
  pure function add_a(i, j) result(sum_ij)
    integer, intent(in) :: i, j
    integer, allocatable :: sum_ij
    sum_ij = 0
  end function add_a
  pure function add_array(i, j) result(sum_ij)
    integer, intent(in), dimension(:) :: i, j
    integer :: sum_ij
    sum_ij = 0
  end function add_array
  pure function add_optional(i, j) result(sum_ij)
    integer, intent(in), optional :: i, j
    integer :: sum_ij
    sum_ij = 0
  end function add_optional
  pure function add_one_value(i, j) result(sum_ij)
    integer, intent(in), value :: i
    integer, intent(in) :: j
    integer :: sum_ij
    sum_ij = 0
  end function add_one_value
  pure function char_one(i, j) result(sum_ij)
    character(8), intent(in) :: i, j
    character(8) :: sum_ij
  end function char_one
  pure function char_two(i, j) result(sum_ij)
    character(4), intent(in) :: i, j
    character(8) :: sum_ij
  end function char_two
  pure function char_three(i, j) result(sum_ij)
    character(8), intent(in) :: i
    character(4), intent(in) :: j
    character(4) :: sum_ij
  end function char_three
  subroutine foo
  end subroutine foo
end
