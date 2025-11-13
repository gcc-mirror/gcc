! { dg-do run }
! { dg-options "-std=f2008 -Wall" }
!
! PR fortran/96255
! Test DO CONCURRENT with optional type specification
! Covers all shadowing scenarios per F2018 19.4(6)

program test_do_concurrent_typespec
  implicit none
  integer :: test_count
  test_count = 0

  ! Test 1: Type-spec with no outer scope variable (BT_UNKNOWN)
  ! Should just set the type, no shadow needed
  call test_no_outer_var()
  test_count = test_count + 1

  ! Test 2: Type-spec shadows outer variable with same kind
  ! Must create shadow per F2018 19.4(6)
  call test_shadow_same_kind()
  test_count = test_count + 1

  ! Test 3: Type-spec shadows outer variable with different kind
  ! Must create shadow per F2018 19.4(6)
  call test_shadow_different_kind()
  test_count = test_count + 1

  ! Test 4: Multiple iterators with mixed scenarios
  call test_multiple_iterators()
  test_count = test_count + 1

contains

  subroutine test_no_outer_var()
    implicit none
    integer :: sum_val

    ! 'j' is not declared in outer scope
    sum_val = 0
    do concurrent (integer :: j = 1:5)
      sum_val = sum_val + j
    end do

    if (sum_val /= 15) stop 1  ! 1+2+3+4+5 = 15
  end subroutine test_no_outer_var

  subroutine test_shadow_same_kind()
    implicit none
    integer :: i
    integer :: outer_val, inner_sum

    ! Set outer 'i' to a specific value
    i = 99
    outer_val = i

    ! DO CONCURRENT with type-spec should shadow 'i'
    ! even though kind is the same
    inner_sum = 0
    do concurrent (integer :: i = 1:3)
      inner_sum = inner_sum + i
    end do

    ! After loop, outer 'i' should be unchanged
    if (i /= outer_val) stop 2
    if (i /= 99) stop 3
    if (inner_sum /= 6) stop 4  ! 1+2+3 = 6
  end subroutine test_shadow_same_kind

  subroutine test_shadow_different_kind()
    implicit none
    integer(kind=4) :: k
    integer :: result

    ! Set outer 'k' to a value
    k = 77

    ! DO CONCURRENT with different kind should shadow
    result = 0
    do concurrent (integer(kind=2) :: k = 1:4)
      result = result + int(k, kind=4)
    end do

    ! Outer 'k' should be unchanged
    if (k /= 77) stop 5
    if (result /= 10) stop 6  ! 1+2+3+4 = 10
  end subroutine test_shadow_different_kind

  subroutine test_multiple_iterators()
    implicit none
    integer :: i, j
    integer :: sum_val

    ! Set outer variables
    i = 100
    j = 200

    ! Multiple iterators: i shadows (same kind), m is new (BT_UNKNOWN)
    ! Per F2018 R1125, ONE type-spec applies to ALL iterators
    sum_val = 0
    do concurrent (integer :: i = 1:2, m = 1:2)
      sum_val = sum_val + i * 10 + m
    end do

    ! Outer i should be unchanged, j should be unchanged
    if (i /= 100) stop 7
    if (j /= 200) stop 8
    ! sum = (1*10+1) + (1*10+2) + (2*10+1) + (2*10+2) = 11+12+21+22 = 66
    if (sum_val /= 66) stop 9
  end subroutine test_multiple_iterators

end program test_do_concurrent_typespec
