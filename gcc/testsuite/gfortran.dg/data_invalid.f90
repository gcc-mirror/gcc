! { dg-do "compile" }
! { dg-options "-std=f95 -fmax-errors=0" }
!
! Testcases from PR fortran/24978
!

SUBROUTINE data_init_scalar_invalid()
  integer :: a
  data       a / 1 /
  data       a / 1 /                             ! { dg-error "re-initialization" }

  integer :: b = 0
  data       b / 1 /                             ! { dg-error "re-initialization" }
END SUBROUTINE

SUBROUTINE data_init_array_invalid()
  ! initialize (at least) one element, re-initialize full array
  integer :: a(3)
  data       a(2) / 2 /
  data       a    / 3*1 /                        ! { dg-error "re-initialization" }

  ! initialize (at least) one element, re-initialize subsection including the element
  integer :: b(3)
  data       b(2)   / 2 /
  data       b(1:2) / 2*1 /                      ! { dg-error "re-initialization" }

  ! initialize subsection, re-initialize (intersecting) subsection
  integer :: c(3)
  data       c(1:2) / 2*1 /
  data       c(2:3) / 1,1 /                      ! { dg-error "re-initialization" }

  ! initialize subsection, re-initialize full array
  integer :: d(3)
  data       d(2:3) / 2*1 /
  data       d      / 2*2, 3 /                   ! { dg-error "re-initialization" }

  ! full array initializer, re-initialize (at least) one element
  integer :: e(3)
  data       e    / 3*1 /
  data       e(2) / 2 /                          ! { dg-error "re-initialization" }

  integer :: f(3) = 0                            ! { dg-error "already is initialized" }
  data       f(2) / 1 /

  ! full array initializer, re-initialize subsection
  integer :: g(3)
  data       g      / 3*1 /
  data       g(1:2) / 2*2 /                      ! { dg-error "re-initialization" }

  integer :: h(3) = 1                            ! { dg-error "already is initialized" }
  data       h(2:3) / 2*2 /

  ! full array initializer, re-initialize full array
  integer :: i(3)
  data       i   / 3*1 /
  data       i   / 2,2,2 /                       ! { dg-error "re-initialization" }

  integer :: j(3) = 1                            ! { dg-error "already is initialized" }
  data       j   / 3*2 /
END SUBROUTINE

SUBROUTINE data_init_matrix_invalid()
  ! initialize (at least) one element, re-initialize full matrix
  integer :: a(3,3)
  data       a(2,2) / 1 /
  data       a      / 9*2 /                      ! { dg-error "re-initialization" }

  ! initialize (at least) one element, re-initialize subsection
  integer :: b(3,3)
  data       b(2,2) / 1 /
  data       b(2,:) / 3*2 /                      ! { dg-error "re-initialization" }

  ! initialize subsection, re-initialize (intersecting) subsection
  integer :: c(3,3)
  data       c(3,:) / 3*1 /, c(:,3) / 3*2 /      ! { dg-error "re-initialization" }

  ! initialize subsection, re-initialize full array
  integer :: d(3,3)
  data       d(2,:) / 1,2,3 /
  data       d      / 9*4 /                      ! { dg-error "re-initialization" }

  ! full array initializer, re-initialize (at least) one element
  integer :: e(3,3)
  data       e      / 9*1 /
  data       e(2,3) / 2 /                        ! { dg-error "re-initialization" }

  integer :: f(3,3) = 1                          ! { dg-error "already is initialized" }
  data       f(3,2) / 2 /

  ! full array initializer, re-initialize subsection
  integer :: g(3,3)
  data       g          / 9 * 1 /
  data       g(2:3,2:3) / 2, 2*3, 4 /            ! { dg-error "re-initialization" }

  integer :: h(3,3) = 1                          ! { dg-error "already is initialized" }
  data       h(2:3,2:3) / 2, 2*3, 4 /

  ! full array initializer, re-initialize full array
  integer :: i(3,3)
  data       i   / 3*1, 3*2, 3*3 /
  data       i   / 9 * 1 /                       ! { dg-error "re-initialization" }

  integer :: j(3,3) = 0                          ! { dg-error "already is initialized" }
  data       j   / 9 * 1 /
END SUBROUTINE

SUBROUTINE data_init_misc_invalid()
  ! wrong number of dimensions
  integer :: a(3)
  data       a(1,1) / 1 /                        ! { dg-error "Rank mismatch" }

  ! index out-of-bounds, direct access
  integer :: b(3)
  data       b(-2) / 1 /                         ! { dg-error "below array lower bound" }

  ! index out-of-bounds, implied do-loop (PR32315)
  integer :: i
  character(len=20), dimension(4) :: string
  data (string(i), i = 1, 5) / 'A', 'B', 'C', 'D', 'E' /   ! { dg-error "above array upper bound" }
END SUBROUTINE

! { dg-excess-errors "" }
