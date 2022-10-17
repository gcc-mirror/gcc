! { dg-do run }
! { dg-additional-sources bind-c-contiguous-3.c }

! Test that multi-dim contiguous is properly handled.

module m
  use iso_c_binding, only: c_intptr_t, c_int
  implicit none (type, external)

interface
  integer(c_intptr_t) function assumed_rank_alloc_c (xx) bind(c)
    import :: c_intptr_t
    integer, allocatable :: xx(..)
  end function
  integer(c_intptr_t) function assumed_rank_pointer_c (xx) bind(c)
    import :: c_intptr_t
    integer, pointer :: xx(..)
  end function
  integer(c_intptr_t) function assumed_rank_c (xx) bind(c)
    import :: c_intptr_t
    integer :: xx(..)
  end function
  integer(c_intptr_t) function assumed_rank_cont_c (xx) bind(c)
    import :: c_intptr_t
    integer, contiguous :: xx(..)
  end function
  integer(c_intptr_t) function assumed_shape_c (xx, num) bind(c)
    import :: c_intptr_t, c_int
    integer :: xx(:,:,:,:)
    integer(c_int), value :: num
  end function
  integer(c_intptr_t) function assumed_shape_cont_c (xx) bind(c)
    import :: c_intptr_t
    integer, contiguous :: xx(:,:,:,:)
  end function
  integer(c_intptr_t) function deferred_shape_alloc_c (xx) bind(c)
    import :: c_intptr_t
    integer, allocatable :: xx(:,:,:,:)
  end function
  integer(c_intptr_t) function deferred_shape_pointer_c (xx) bind(c)
    import :: c_intptr_t
    integer, pointer :: xx(:,:,:,:)
  end function

end interface

contains

integer function get_n (idx, lbound, extent) result(res)
  integer, contiguous :: idx(:), lbound(:), extent(:)
  integer :: i
  if (size(idx) /= size(lbound) .or. size(idx) /= size(extent)) &
    error stop 20
  res = idx(1) - lbound(1) + 1
  do i = 2, size(idx)
    res = res + product(extent(:i-1)) * (idx(i)-lbound(i))
  end do
end

integer(c_intptr_t) function assumed_rank_alloc_f (xx) bind(c) result(res)
  integer, allocatable :: xx(..)
  integer :: i, j, k, l, lb(4)
  select rank (xx)
  rank (4)
    do l = lbound(xx, dim=4), ubound(xx, dim=4)
      do k = lbound(xx, dim=3), ubound(xx, dim=3)
        do j = lbound(xx, dim=2), ubound(xx, dim=2)
          do i = lbound(xx, dim=1), ubound(xx, dim=1)
            xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
          end do
        end do
      end do
    end do
    lb = lbound(xx)
    res = %loc(xx(lb(1),lb(2),lb(3),lb(4)))  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
end

integer(c_intptr_t) function assumed_rank_pointer_f (xx) bind(c) result(res)
  integer, pointer :: xx(..)
  integer :: i, j, k, l, lb(4)
  select rank (xx)
  rank (4)
    do l = lbound(xx, dim=4), ubound(xx, dim=4)
      do k = lbound(xx, dim=3), ubound(xx, dim=3)
        do j = lbound(xx, dim=2), ubound(xx, dim=2)
          do i = lbound(xx, dim=1), ubound(xx, dim=1)
            xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
          end do
        end do
      end do
    end do
    lb = lbound(xx)
    res = %loc(xx(lb(1),lb(2),lb(3),lb(4)))  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
end


integer(c_intptr_t) function assumed_rank_f (xx) bind(c) result(res)
  integer :: xx(..)
  integer :: i, j, k, l
  select rank (xx)
  rank (4)
    do l = 1, size(xx, dim=4)
      do k = 1, size(xx, dim=3)
        do j = 1, size(xx, dim=2)
          do i = 1, size(xx, dim=1)
            xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
          end do
        end do
      end do
    end do
    res = %loc(xx(1,1,1,1))  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
end

integer(c_intptr_t) function assumed_rank_cont_f (xx) bind(c) result(res)
  integer, contiguous :: xx(..)
  integer :: i, j, k, l
  select rank (xx)
  rank (4)
    do l = 1, size(xx, dim=4)
      do k = 1, size(xx, dim=3)
        do j = 1, size(xx, dim=2)
          do i = 1, size(xx, dim=1)
            xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
          end do
        end do
      end do
    end do
    res = %loc(xx(1,1,1,1))  ! { dg-warning "Legacy Extension" }
  rank default
    error stop 99
  end select
end

integer(c_intptr_t) function assumed_shape_f (xx) bind(c) result(res)
  integer :: xx(:,:,:,:)
  integer :: i, j, k, l
  do l = 1, ubound(xx, dim=4)
    do k = 1, ubound(xx, dim=3)
      do j = 1, ubound(xx, dim=2)
        do i = 1, ubound(xx, dim=1)
          xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
        end do
      end do
    end do
  end do
  res = %loc(xx(1,1,1,1))  ! { dg-warning "Legacy Extension" }
end

integer(c_intptr_t) function assumed_shape2_f (xx, n) bind(c) result(res)
  integer, value :: n
  integer :: xx(-n:, -n:, -n:, -n:)
  integer :: i, j, k, l
  do l = -n, ubound(xx, dim=4)
    do k = -n, ubound(xx, dim=3)
      do j = -n, ubound(xx, dim=2)
        do i = -n, ubound(xx, dim=1)
          xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
        end do
      end do
    end do
  end do
  res = %loc(xx(-n,-n,-n,-n))  ! { dg-warning "Legacy Extension" }
end

integer(c_intptr_t) function assumed_shape_cont_f (xx) bind(c) result(res)
  integer, contiguous :: xx(:,:,:,:)
  integer :: i, j, k, l
  do l = 1, ubound(xx, dim=4)
    do k = 1, ubound(xx, dim=3)
      do j = 1, ubound(xx, dim=2)
        do i = 1, ubound(xx, dim=1)
          xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
        end do
      end do
    end do
  end do
  res = %loc(xx(1,1,1,1))  ! { dg-warning "Legacy Extension" }
end

integer(c_intptr_t) function assumed_shape2_cont_f (xx, n) bind(c) result(res)
  integer, value :: n
  integer, contiguous :: xx(-n:, -n:, -n:, -n:)
  integer :: i, j, k, l
  do l = -n, ubound(xx, dim=4)
    do k = -n, ubound(xx, dim=3)
      do j = -n, ubound(xx, dim=2)
        do i = -n, ubound(xx, dim=1)
          xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
        end do
      end do
    end do
  end do
  res = %loc(xx(-n,-n,-n,-n))  ! { dg-warning "Legacy Extension" }
end

integer(c_intptr_t) function deferred_shape_alloc_f (xx) bind(c) result(res)
  integer, allocatable :: xx(:,:,:,:)
  integer :: i, j, k, l, lb(4)
  do l = lbound(xx, dim=4), ubound(xx, dim=4)
    do k = lbound(xx, dim=3), ubound(xx, dim=3)
      do j = lbound(xx, dim=2), ubound(xx, dim=2)
        do i = lbound(xx, dim=1), ubound(xx, dim=1)
          xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
        end do
      end do
    end do
  end do
  lb = lbound(xx)
  res = %loc(xx(lb(1),lb(2),lb(3),lb(4)))  ! { dg-warning "Legacy Extension" }
end

integer(c_intptr_t) function deferred_shape_pointer_f (xx) bind(c) result(res)
  integer, pointer :: xx(:,:,:,:)
  integer :: i, j, k, l, lb(4)
  do l = lbound(xx, dim=4), ubound(xx, dim=4)
    do k = lbound(xx, dim=3), ubound(xx, dim=3)
      do j = lbound(xx, dim=2), ubound(xx, dim=2)
        do i = lbound(xx, dim=1), ubound(xx, dim=1)
          xx(i,j,k,l) = -get_n([i,j,k,l], lbound(xx), shape(xx))
        end do
      end do
    end do
  end do
  lb = lbound(xx)
  res = %loc(xx(lb(1),lb(2),lb(3),lb(4)))  ! { dg-warning "Legacy Extension" }
end
end module


use m
implicit none (type, external)
integer, dimension(10,10,10,10) :: var_init, var
target :: var
integer, allocatable, dimension(:,:,:,:) :: a1, a2
integer, pointer, dimension(:,:,:,:) :: p1, p2
integer(c_intptr_t) :: loc4
integer :: i, k, j, l, cnt

do l = 1, ubound(var_init, dim=4)
  do k = 1, ubound(var_init, dim=3)
    do j = 1, ubound(var_init, dim=2)
      do i = 1, ubound(var_init, dim=1)
        var_init(i,j,k,l) = get_n([i,j,k,l], lbound(var_init), shape(var_init))
      end do
    end do
  end do
end do

! Fortran calls

! ----- allocatable + pointer dummies -------

allocate(a1, mold=var_init)
allocate(p1, mold=var_init)
allocate(a2(-5:4,-10:-1,1:10,11:20))
allocate(p2(-5:4,-10:-1,1:10,11:20))

a1(:,:,:,:) = var_init
loc4 = assumed_rank_alloc_f (a1)
cnt = size(a1) - check_unmod (a1)
call check  (a1, loc4, .true., cnt)
call check2 (a1)

a2(:,:,:,:) = var_init
loc4 = assumed_rank_alloc_f (a2)
cnt = size(a2) - check_unmod (a2)
call check  (a2, loc4, .true., cnt)
call check2 (a2)

a1(:,:,:,:) = var_init
loc4 = deferred_shape_alloc_f (a1)
cnt = size(a1) - check_unmod (a1)
call check  (a1, loc4, .true., cnt)
call check2 (a1)

a2(:,:,:,:) = var_init
loc4 = deferred_shape_alloc_f (a2)
cnt = size(a2) - check_unmod (a2)
call check  (a2, loc4, .true., cnt)
call check2 (a2)

deallocate(a1, a2)

p1(:,:,:,:) = var_init
loc4 = assumed_rank_pointer_f (p1)
cnt = size(p1) - check_unmod (p1)
call check  (p1, loc4, .true., cnt)
call check2 (p1)

p2(:,:,:,:) = var_init
loc4 = assumed_rank_pointer_f (p2)
cnt = size(p2) - check_unmod (p2)
call check  (p2, loc4, .true., cnt)
call check2 (p2)

p1(:,:,:,:) = var_init
loc4 = deferred_shape_pointer_f (p1)
cnt = size(p1) - check_unmod (p1)
call check  (p1, loc4, .true., cnt)
call check2 (p1)

p2(:,:,:,:) = var_init
loc4 = deferred_shape_pointer_f (p2)
cnt = size(p2) - check_unmod (p2)
call check  (p2, loc4, .true., cnt)
call check2 (p2)

deallocate(p1, p2)

! --- p => var(4:7,::3,::2,:)
var = var_init
p1 => var(4:7,::3,::2,:)
loc4 = assumed_rank_pointer_f (p1)
cnt = size(p1) - check_unmod (p1)
call check  (p1, loc4, .false., cnt)
call check2 (p1)

var = var_init
p2(-5:,-10:,1:,11:) => var(4:7,::3,::2,:)
loc4 = assumed_rank_pointer_f (p2)
cnt = size(p2) - check_unmod (p2)
call check  (p2, loc4, .false., cnt)
call check2 (p2)

var = var_init
p1 => var(4:7,::3,::2,:)
loc4 = deferred_shape_pointer_f (p1)
cnt = size(p1) - check_unmod (p1)
call check  (p1, loc4, .false., cnt)
call check2 (p1)

var = var_init
p2(-5:,-10:,1:,11:) => var(4:7,::3,::2,:)
loc4 = deferred_shape_pointer_f (p2)
cnt = size(p2) - check_unmod (p2)
call check  (p2, loc4, .false., cnt)
call check2 (p2)



! ----- nonallocatable + nonpointer dummies -------

var = var_init
loc4 = assumed_rank_f (var)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .false., cnt)
call check2 (var)

var = var_init
loc4 = assumed_shape_f (var)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .false., cnt)
call check2 (var)

var = var_init
loc4 = assumed_shape2_f (var, 99)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .false., cnt)
call check2 (var)

var = var_init
loc4 = assumed_rank_cont_f (var)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .true., cnt)
call check2 (var)

var = var_init
loc4 = assumed_shape_cont_f (var)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .true., cnt)
call check2 (var)

var = var_init
loc4 = assumed_shape2_cont_f (var, 99)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .true., cnt)
call check2 (var)

! --- var(4:7,::3,::2,:)

var = var_init
loc4 = assumed_rank_f (var(4:7,::3,::2,:))
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .false., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
loc4 = assumed_shape_f (var(4:7,::3,::2,:))
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .false., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
loc4 = assumed_shape2_f (var(4:7,::3,::2,:), 99)
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .false., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
loc4 = assumed_rank_cont_f (var(4:7,::3,::2,:))
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .true., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
loc4 = assumed_shape_cont_f (var(4:7,::3,::2,:))
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .true., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
loc4 = assumed_shape2_cont_f (var(4:7,::3,::2,:), 99)
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .true., cnt)
call check2 (var(4:7,::3,::2,:))


! C calls

! ----- allocatable + pointer dummies -------

allocate(a1, mold=var_init)
allocate(p1, mold=var_init)
allocate(a2(-5:4,-10:-1,1:10,11:20))
allocate(p2(-5:4,-10:-1,1:10,11:20))

a1(:,:,:,:) = var_init
loc4 = assumed_rank_alloc_c (a1)
cnt = size(a1) - check_unmod (a1)
call check  (a1, loc4, .true., cnt)
call check2 (a1)

a2(:,:,:,:) = var_init
loc4 = assumed_rank_alloc_c (a2)
cnt = size(a2) - check_unmod (a2)
call check  (a2, loc4, .true., cnt)
call check2 (a2)

a1(:,:,:,:) = var_init
loc4 = deferred_shape_alloc_c (a1)
cnt = size(a1) - check_unmod (a1)
call check  (a1, loc4, .true., cnt)
call check2 (a1)

a2(:,:,:,:) = var_init
loc4 = deferred_shape_alloc_c (a2)
cnt = size(a2) - check_unmod (a2)
call check  (a2, loc4, .true., cnt)
call check2 (a2)

deallocate(a1, a2)

p1(:,:,:,:) = var_init
loc4 = assumed_rank_pointer_c (p1)
cnt = size(p1) - check_unmod (p1)
call check  (p1, loc4, .true., cnt)
call check2 (p1)

p2(:,:,:,:) = var_init
loc4 = assumed_rank_pointer_c (p2)
cnt = size(p2) - check_unmod (p2)
call check  (p2, loc4, .true., cnt)
call check2 (p2)

p1(:,:,:,:) = var_init
loc4 = deferred_shape_pointer_c (p1)
cnt = size(p1) - check_unmod (p1)
call check  (p1, loc4, .true., cnt)
call check2 (p1)

p2(:,:,:,:) = var_init
loc4 = deferred_shape_pointer_c (p2)
cnt = size(p2) - check_unmod (p2)
call check  (p2, loc4, .true., cnt)
call check2 (p2)

deallocate(p1, p2)

! --- p => var(4:7,::3,::2,:)
var = var_init
p1 => var(4:7,::3,::2,:)
loc4 = assumed_rank_pointer_c (p1)
cnt = size(p1) - check_unmod (p1)
call check  (p1, loc4, .false., cnt)
call check2 (p1)

var = var_init
p2(-5:,-10:,1:,11:) => var(4:7,::3,::2,:)
loc4 = assumed_rank_pointer_c (p2)
cnt = size(p2) - check_unmod (p2)
call check  (p2, loc4, .false., cnt)
call check2 (p2)

var = var_init
p1 => var(4:7,::3,::2,:)
loc4 = deferred_shape_pointer_c (p1)
cnt = size(p1) - check_unmod (p1)
call check  (p1, loc4, .false., cnt)
call check2 (p1)

var = var_init
p2(-5:,-10:,1:,11:) => var(4:7,::3,::2,:)
loc4 = deferred_shape_pointer_c (p2)
cnt = size(p2) - check_unmod (p2)
call check  (p2, loc4, .false., cnt)
call check2 (p2)


! ----- nonallocatable + nonpointer dummies -------

var = var_init
loc4 = assumed_rank_c (var)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .false., cnt)
call check2 (var)

var = var_init
! calls assumed_shape_f
loc4 = assumed_shape_c (var, num=1)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .false., cnt)
call check2 (var)

var = var_init
! calls assumed_shape_cont_f
loc4 = assumed_shape_c (var, num=2)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .true., cnt)
call check2 (var)

var = var_init
! calls assumed_rank_cont_f
loc4 = assumed_shape_c (var, num=3)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .true., cnt)
call check2 (var)

var = var_init
loc4 = assumed_rank_cont_c (var)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .true., cnt)
call check2 (var)

var = var_init
loc4 = assumed_shape_cont_c (var)
cnt = size(var) - check_unmod (var)
call check  (var, loc4, .true., cnt)
call check2 (var)

! --- var(4:7,::3,::2,:)

var = var_init
loc4 = assumed_rank_c (var(4:7,::3,::2,:))
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .false., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
! calls assumed_shape_f
loc4 = assumed_shape_c (var(4:7,::3,::2,:), num=4)
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .false., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
! calls assumed_shape_cont_f
loc4 = assumed_shape_c (var(4:7,::3,::2,:), num=5)
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .true., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
! calls assumed_rank_cont_f
loc4 = assumed_shape_c (var(4:7,::3,::2,:), num=6)
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .true., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
loc4 = assumed_rank_cont_c (var(4:7,::3,::2,:))
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .true., cnt)
call check2 (var(4:7,::3,::2,:))

var = var_init
loc4 = assumed_shape_cont_c (var(4:7,::3,::2,:))
cnt = size(var) - check_unmod (var)
call check  (var(4:7,::3,::2,:), loc4, .true., cnt)
call check2 (var(4:7,::3,::2,:))


contains

! Ensure that the rest is still okay
! Returns the number of elements >= 0
integer function check_unmod (x) result(cnt)
  integer, contiguous, intent(in) ::  x(:,:,:,:)
  integer :: i, k, j, l
  cnt = 0
  do l = 1, ubound(x, dim=4)
    do k = 1, ubound(x, dim=3)
      do j = 1, ubound(x, dim=2)
        do i = 1, ubound(x, dim=1)
          if (x(i,j,k,l) >= 0) then
            cnt = cnt + 1
            if (x(i,j,k,l) /= get_n([i,j,k,l], lbound(x), shape(x))) &
              error stop 5
          endif
        end do
      end do
    end do
  end do
end

subroutine check(x, loc1, cont, cnt)
  integer, intent(in) :: x(:,:,:,:)
  integer(c_intptr_t), intent(in), optional :: loc1
  logical, intent(in), optional :: cont ! dummy has CONTIGUOUS attr
  integer, intent(in), optional :: cnt
  integer(c_intptr_t) :: loc2
  integer :: i, k, j, l
  if (present (loc1)) then
    loc2 = %loc(x(1,1,1,1))  ! { dg-warning "Legacy Extension" }
    if (is_contiguous (x) .or. .not.cont) then
      if (loc1 /= loc2) error stop 1
    else
      if (loc1 == loc2) error stop 2
    end if
    if (cnt /= size(x)) error stop 3
  end if
  do l = 1, ubound(x, dim=4)
    do k = 1, ubound(x, dim=3)
      do j = 1, ubound(x, dim=2)
        do i = 1, ubound(x, dim=1)
          if (x(i,j,k,l) /= -get_n([i,j,k,l], lbound(x), shape(x))) &
            error stop 4
        end do
      end do
    end do
  end do
end

subroutine check2(x)
  integer, contiguous, intent(in) :: x(:,:,:,:)
  call check(x)
end subroutine
end
