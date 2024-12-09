! { dg-do run }
!
! Test transformational intrinsics with class results - PR102689
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module tests
  type t
    integer :: i
  end type t
  type, extends(t) :: s
    integer :: j
  end type

contains

  subroutine class_bar(x)
    class(*), intent(in) :: x(..)
    integer :: checksum

    if (product (shape (x)) .ne. 10) stop 1
    select rank (x)
      rank (1)
        select type (x)
          type is (s)
            if (sum(x%i) .ne. 55) stop 2
            if ((sum(x%j) .ne. 550) .and. (sum(x%j) .ne. 110)) stop 3
          type is (character(*))
            checksum = sum(ichar(x(:)(1:1)) + ichar(x(:)(2:2)))
            if ((checksum .ne. 1490) .and. (checksum .ne. 2130)) stop 4
          class default
            stop
        end select
      rank (2)
        select type (x)
          type is (s)
            if (sum(x%i) .ne. 55) stop 5
            if (sum(x%j) .ne. 550) stop 6
          type is (character(*));
            checksum = sum(ichar(x(:,:)(1:1)) + ichar(x(:,:)(2:2)))
            if ((checksum .ne. 1490) .and. (checksum .ne. 2130)) stop 7
          class default
            stop 8
        end select
      rank (3)
        select type (x)
          type is (s)
            if (sum(x%i) .ne. 55) stop 9
            if ((sum(x%j) .ne. 550) .and. (sum(x%j) .ne. 110)) stop 10
          type is (character(*))
            checksum = sum(ichar(x(:,:,:)(1:1)) + ichar(x(:,:,:)(2:2)))
            if ((checksum .ne. 1490) .and. (checksum .ne. 2130)) stop 11
          class default
            stop 12
        end select
      end select
  end
end module tests

Module class_tests
  use tests
  implicit none
  private
  public :: test_class

  integer :: j
  integer :: src(10)
  type (s), allocatable :: src3 (:,:,:)
  class(t), allocatable :: B(:,:,:), D(:)

! gfortran gave type(t) for D for all these test cases.
contains

  subroutine test_class

    src3 = reshape ([(s(j,j*10), j=1,10)], [1,10,1])
    call test1                               ! Now D OK for gfc15. B OK back to gfc10
    call foo

    call class_rebar(reshape(B, [10]))       ! This is the original failure - run time segfault

    deallocate (B, D)

    allocate(B(2,1,5), source = s(1,11))    ! B was OK but descriptor elem_len = 4 so....
    src = [(j, j=1,10)]
    call test2                              ! D%j was type(t) and filled with B[1:5]
    call foo
    deallocate (B,D)

    call test3                              ! B is set to type(t) and filled with [s(1,11)..s(5,50)]
    call foo
    deallocate (B,D)

    B = src3                                ! Now D was like B in test3. B OK back to gfc10
    call foo
    deallocate (B, D)
    if (allocated (src3)) deallocate (src3)
  end

  subroutine class_rebar (arg)
    class(t) :: arg(:)
    call class_bar (arg)
  end

  subroutine test1
    allocate(B, source = src3)
  end

  subroutine test2
    B%i = RESHAPE(src, shape(B))
  end

  subroutine test3
    B = reshape ([(s(j,j*10), j=1,10)], shape(B))
  end

  subroutine foo
    D = reshape(B, [10])
    call class_bar(B)
    call class_bar(D)
  end
end module class_tests

module unlimited_tests
  use tests
  implicit none
  private
  public :: test_unlimited

  integer :: j
  character(len = 2, kind = 1) :: chr(10)
  character(len = 2, kind = 1) :: chr3(5, 2, 1)
  type (s), allocatable :: src3 (:,:,:)
  class(*), allocatable :: B(:,:,:), D(:)

contains
  subroutine test_unlimited
    call test1
    call foo

    call unlimited_rebar(reshape(B, [10]))       ! Unlimited version of the original failure

    deallocate (B, D)

    call test3
    call foo
    deallocate (B,D)

    B = src3
    call foo
    deallocate (B, D)

    B = reshape ([(char(64 + 2*j - 1)//char(64 + 2*j), j = 1,10)], [5, 1, 2])
    call foo
    deallocate (B, D)

    chr = [(char(96 + 2*j - 1)//char(96 + 2*j), j = 1,10)]
    B = reshape (chr, [5, 1, 2])
    call foo

    call unlimited_rebar(reshape(B, [10]))       ! Unlimited/ character version of the original failure
    deallocate (B, D)

    chr3 = reshape (chr, shape(chr3))
    B = chr3
    call foo
    deallocate (B, D)
    if (allocated (src3)) deallocate (src3)
  end

  subroutine unlimited_rebar (arg)
    class(*), allocatable :: arg(:)              ! Not having this allocatable => pr117901
    call class_bar (arg)
  end

  subroutine test1
    src3 = reshape ([(s(j,j*10), j=1,10)], [2,1,5])
    allocate(B, source = src3)
  end

  subroutine test3
    B = reshape ([(s(j,j*10), j=1,10)], shape(B))
  end

  subroutine foo
    D = reshape(B, [10])
    call class_bar(B)
    call class_bar(D)
  end

end module unlimited_tests

  call t1
  call t2
contains
  subroutine t1
    use class_tests
    call test_class
  end
  subroutine t2
    use unlimited_tests
    call test_unlimited
  end
end
