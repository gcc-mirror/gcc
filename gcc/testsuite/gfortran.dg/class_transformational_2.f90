! { dg-do run }
!
! Test transformational intrinsics other than reshape with class results.
! This emerged from PR102689, for which class_transformational_1.f90 tests
! class-valued reshape.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  type t
    integer :: i
  end type t
  type, extends(t) :: s
    integer :: j
  end type
  class(t), allocatable :: a(:), aa(:), b(:,:), c(:,:,:), field(:,:,:)
  integer, allocatable :: ishape(:), ii(:), ij(:)
  logical :: la(2), lb(2,2), lc (4,2,2)
  integer :: j, stop_flag

  call check_spread
  call check_pack
  call check_unpack
  call check_eoshift
  call check_eoshift_dep
  deallocate (a, aa, b, c, field, ishape, ii, ij)
contains
  subroutine check_result_a (shift)
    type (s), allocatable :: ss(:)
    integer :: shift
    select type (aa)
      type is (s)
        ss = eoshift (aa, shift = shift, boundary = aa(1), dim = 1)
        ishape = shape (aa);
        ii = ss%i
        ij = ss%j
    end select
    if (any (ishape .ne. shape (a))) stop stop_flag + 1
    select type (a)
      type is (s)
        if (any (a%i .ne. ii)) stop stop_flag + 2
        if (any (a%j .ne. ij)) stop stop_flag + 3
      class default
        stop stop_flag + 4
    end select
  end

  subroutine check_result
    if (any (shape (c) .ne. ishape)) stop stop_flag + 1
    select type (a)
      type is (s)
        if (any (a%i .ne. ii)) stop stop_flag + 2
        if (any (a%j .ne. ij)) stop stop_flag + 3
      class default
        stop stop_flag + 4
    end select
  end

  subroutine check_spread
    stop_flag = 10
    a = [(s(j,10*j), j = 1,2)]
    b = spread (a, dim = 2, ncopies = 2)
    c = spread (b, dim = 1, ncopies = 4)
    a = reshape (c, [size (c)])
    ishape = [4,2,2]
    ii = [1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2]
    ij = 10*[1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2]
    call check_result
  end

  subroutine check_pack
    stop_flag = 20
    la = [.false.,.true.]
    lb = spread (la, dim = 2, ncopies = 2)
    lc = spread (lb, dim = 1, ncopies = 4)
    a = pack (c, mask = lc)
    ishape = shape (lc)
    ii = [2,2,2,2,2,2,2,2]
    ij = 10*[2,2,2,2,2,2,2,2]
    call check_result
  end

  subroutine check_unpack
    stop_flag = 30
    a = [(s(j,10*j), j = 1,16)]
    field = reshape ([(s(100*j,1000*j), j = 1,16)], shape(lc))
    c = unpack (a, mask = lc, field = field)
    a = reshape (c, [product (shape (lc))])
    ishape = shape (lc)
    ii = [100,200,300,400,1,2,3,4,900,1000,1100,1200,5,6,7,8]
    ij = [1000,2000,3000,4000,10,20,30,40,9000,10000, 11000,12000,50,60,70,80]
    call check_result
  end

  subroutine check_eoshift
    stop_flag = 40
    aa = a
    a = eoshift (aa, shift = 3, boundary = aa(1), dim = 1)
    call check_result_a (3)
  end

  subroutine check_eoshift_dep
    stop_flag = 50
    aa = a
    a = eoshift (a, shift = -3, boundary = a(1), dim = 1)
    call check_result_a (-3)
  end
end
