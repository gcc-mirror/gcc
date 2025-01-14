! { dg-do compile }
! { dg-options "-fcoarray=single" }

module m
  type t1
    integer, allocatable :: x
  end type t1

  type t2
    type(t1), allocatable :: y
  end type t2

  type, abstract :: abstract_type
  end type abstract_type

contains
  subroutine test_c1130(a, b, c, d, e, f, g, h, j)
    integer, allocatable :: a
    integer, intent(in) :: b
    integer, optional :: c
    type(t1) :: d
    real :: e[*]
    integer :: f(*)
    type(t2) :: g
    class(abstract_type), pointer :: h
    class(abstract_type) :: j
    integer :: i

    ! C1130 tests
    do concurrent (i=1:5) local(a)  ! { dg-error "ALLOCATABLE attribute not permitted for 'a' in LOCAL locality-spec" }
    end do
    do concurrent (i=1:5) local(b)  ! { dg-error "Dummy argument 'b' with INTENT\\(IN\\) in variable definition context \\(LOCAL\\) at" }
    end do
    do concurrent (i=1:5) local(c)  ! { dg-error "OPTIONAL attribute not permitted for 'c' in LOCAL locality-spec" }
    end do
    do concurrent (i=1:5) local(d)  ! { dg-error "Type with ultimate allocatable component not permitted for 'd' in LOCAL locality-spec" }
    end do
    do concurrent (i=1:5) local(e)  ! { dg-error "Expected variable name in LOCAL locality spec" }
    end do
    do concurrent (i=1:5) local(f)  ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array 'f'" }
    end do
    do concurrent (i=1:5) local(g)  ! { dg-error "Type with ultimate allocatable component not permitted for 'g' in LOCAL locality-spec" }
    end do
    do concurrent (i=1:5) local(h)
    end do
    do concurrent (i=1:5) local(j)  ! { dg-error "Nonpointer polymorphic dummy argument not permitted for 'j' in LOCAL locality-spec" }
    end do

    ! LOCAL_INIT tests
    do concurrent (i=1:5) local_init(a)  ! { dg-error "ALLOCATABLE attribute not permitted for 'a' in LOCAL_INIT locality-spec" }
    end do
    do concurrent (i=1:5) local_init(b)  ! { dg-error "Dummy argument 'b' with INTENT\\(IN\\) in variable definition context \\(LOCAL_INIT\\) at" }
    end do
    do concurrent (i=1:5) local_init(c)  ! { dg-error "OPTIONAL attribute not permitted for 'c' in LOCAL_INIT locality-spec" }
    end do
    do concurrent (i=1:5) local_init(d)  ! { dg-error "Type with ultimate allocatable component not permitted for 'd' in LOCAL_INIT locality-spec" }
    end do
    do concurrent (i=1:5) local_init(e)  ! { dg-error "Expected variable name in LOCAL_INIT locality spec" }
    end do
    do concurrent (i=1:5) local_init(f)  ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array 'f'" }
    end do
    do concurrent (i=1:5) local_init(g)  ! { dg-error "Type with ultimate allocatable component not permitted for 'g' in LOCAL_INIT locality-spec" }
    end do
    do concurrent (i=1:5) local_init(h)
    end do
    do concurrent (i=1:5) local_init(j)  ! { dg-error "Nonpointer polymorphic dummy argument not permitted for 'j' in LOCAL_INIT locality-spec" }
    end do
  end subroutine test_c1130

  subroutine test_c1131(a, b, c, d, e, f, g)
    integer, asynchronous :: a
    integer, intent(in) :: b
    integer, optional :: c
    integer, volatile :: d
    real :: e[*]
    integer :: f(*)
    real :: g(3)[*]
    integer :: i

    ! C1131 tests
    do concurrent (i=1:5) reduce(+:a)  ! { dg-error "ASYNCHRONOUS attribute not permitted for 'a' in REDUCE locality-spec" }
    end do
    do concurrent (i=1:5) reduce(+:b)
    ! { dg-error "Dummy argument 'b' with INTENT\\(IN\\) in variable definition context \\(REDUCE\\)" "" { target *-*-* } .-1 }
    end do
    do concurrent (i=1:5) reduce(+:c)  ! { dg-error "OPTIONAL attribute not permitted for 'c' in REDUCE locality-spec" }
    end do
    do concurrent (i=1:5) reduce(+:d)  ! { dg-error "VOLATILE attribute not permitted for 'd' in REDUCE locality-spec" }
    end do
    do concurrent (i=1:5) reduce(+:e)  ! { dg-error "Expected variable name in REDUCE locality spec" }
    end do
    do concurrent (i=1:5) reduce(+:f)  ! { dg-error "The upper bound in the last dimension must appear in the reference to the assumed size array 'f'" }
    end do
    do concurrent (i=1:5) reduce(+:g(2)[1])  ! { dg-error "Expected variable name in REDUCE locality spec" }
    end do
  end subroutine test_c1131

  subroutine test_c1132()
    logical :: l1, l2, l3, l4
    integer :: i, int1
    real :: r1
    complex :: c1, c2, c3
    character(len=10) :: str1, str2, str3, str4

    ! C1132 tests
    do concurrent (i=1:5) &
      reduce(+:l1) & ! { dg-error "Expected numeric type for 'l1' in REDUCE at \\(1\\), got LOGICAL" }
      reduce(*:l2) & ! { dg-error "Expected numeric type for 'l2' in REDUCE at \\(1\\), got LOGICAL" }
      reduce(max:l3) & ! { dg-error "Expected INTEGER, REAL or CHARACTER type for 'l3' in REDUCE with MIN/MAX at \\(1\\), got LOGICAL" }
      reduce(iand:l4) ! { dg-error "Expected integer type for 'l4' in REDUCE with IAND/IOR/IEOR at \\(1\\), got LOGICAL" }
    end do

    do concurrent (i=1:5) &
      reduce(*:str2) & ! { dg-error "Expected numeric type for 'str2' in REDUCE at \\(1\\), got CHARACTER" }
      reduce(min:str3) & ! OK
      reduce(max:str4) ! OK
    end do

    do concurrent (i=1:5) &
      reduce(*:c2) & ! OK
      reduce(max:c3) ! { dg-error "Expected INTEGER, REAL or CHARACTER type for 'c3' in REDUCE with MIN/MAX at \\(1\\), got COMPLEX" }
    end do

  end subroutine test_c1132

end module m