! { dg-do run }
! { dg-additional-options "-fdump-tree-original -fsanitize=undefined" }
!
! PR fortran/101135 - Load of null pointer when passing absent
! assumed-shape array argument for an optional dummy argument
!
! Based on testcase by Marcel Jacobse

program main
  implicit none
  character(len=3) :: a(6) = ['abc', 'def', 'ghi', 'jlm', 'nop', 'qrs']
  call as ()
  call as (a(::2))
  call as_c ()
  call as_c (a(2::2))
  call test_wrapper
  call test_wrapper_c
  call test_ar_wrapper
  call test_ar_wrapper_c
contains
  subroutine as (xx)
    character(len=*), optional, intent(in) :: xx(*)
    if (.not. present (xx)) return
    print *, xx(1:3)
  end subroutine as

  subroutine as_c (zz) bind(c)
    character(len=*), optional, intent(in) :: zz(*)
    if (.not. present (zz)) return
    print *, zz(1:3)
  end subroutine as_c

  subroutine test_wrapper (x)
    real, dimension(1), intent(out), optional :: x
    call test (x)
    call test1 (x)
    call test_c (x)
    call test1_c (x)
  end subroutine test_wrapper

  subroutine test_wrapper_c (w) bind(c)
    real, dimension(1), intent(out), optional :: w
    call test (w)
    call test1 (w)
    call test_c (w)
    call test1_c (w)
  end subroutine test_wrapper_c

  subroutine test (y)
    real, dimension(:), intent(out), optional :: y
    if (present (y)) y=0.
  end subroutine test

  subroutine test_c (y) bind(c)
    real, dimension(:), intent(out), optional :: y
    if (present (y)) y=0.
  end subroutine test_c

  subroutine test1 (y)
    real, dimension(1), intent(out), optional :: y
    if (present (y)) y=0.
  end subroutine test1

  subroutine test1_c (y) bind(c)
    real, dimension(1), intent(out), optional :: y
    if (present (y)) y=0.
  end subroutine test1_c

  subroutine test_ar_wrapper (p, q, r)
    real,               intent(out), optional :: p
    real, dimension(1), intent(out), optional :: q
    real, dimension(:), intent(out), optional :: r
    call test_ar (p)
    call test_ar (q)
    call test_ar (r)
    call test_ar_c (p)
    call test_ar_c (q)
    call test_ar_c (r)
  end subroutine test_ar_wrapper

  subroutine test_ar_wrapper_c (u, v, s) bind(c)
    real,               intent(out), optional :: u
    real, dimension(1), intent(out), optional :: v
    real, dimension(:), intent(out), optional :: s
    call test_ar (u)
    call test_ar (v)
!   call test_ar (s)    ! Disabled due to runtime segfault, see pr114355
    call test_ar_c (u)
    call test_ar_c (v)
    call test_ar_c (s)
  end subroutine test_ar_wrapper_c

  subroutine test_ar (z)
    real, dimension(..), intent(out), optional :: z
  end subroutine test_ar

  subroutine test_ar_c (z) bind(c)
    real, dimension(..), intent(out), optional :: z
  end subroutine test_ar_c
end program

! { dg-final { scan-tree-dump-times "data = v != 0B " 2 "original" } }
! { dg-final { scan-tree-dump-times "data = w != 0B " 2 "original" } }
! { dg-final { scan-tree-dump-times "data = q != 0B " 2 "original" } }
! { dg-final { scan-tree-dump-times "data = x != 0B " 2 "original" } }
! { dg-final { scan-tree-dump-times "data = xx.0 != 0B " 1 "original" } }
! { dg-output " abcghinop(\n|\r\n|\r)" }"
! { dg-output " defjlmqrs(\n|\r\n|\r)" }"
