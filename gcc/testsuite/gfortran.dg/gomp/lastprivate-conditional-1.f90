subroutine foo (p)
  implicit none
  logical :: p(:)
  integer a, b, c, d, e, f, g, h;
  integer :: i
  a = -1; b = -1; c = -1; d = -1; e = -1; f = -1; g = -1; h = -1
  !$omp teams
    !$omp distribute lastprivate (conditional: a) ! { dg-error "conditional 'lastprivate' clause on 'distribute' construct" }
    do i = 1, 32
      if (p(i)) &
        a = i
    end do
    !$omp distribute simd lastprivate (conditional: b) ! { dg-error "conditional 'lastprivate' clause on 'distribute' construct" }
    do i = 1, 32
      if (p(i)) &
        b = i
    end do
    !$omp distribute parallel do lastprivate (conditional: c) ! { dg-error "conditional 'lastprivate' clause on 'distribute' construct" }
    do i = 1, 32
      if (p(i)) &
        c = i
    end do
    !$omp distribute parallel do simd lastprivate (conditional: d) ! { dg-error "conditional 'lastprivate' clause on 'distribute' construct" }
    do i = 1, 32
      if (p(i)) &
        d = i
    end do
  !$omp end teams

  !$omp teams distribute parallel do lastprivate (conditional: e) ! { dg-error "conditional 'lastprivate' clause on 'distribute' construct" }
  do i = 1, 32
    if (p(i)) &
      e = i
  end do

  !$omp parallel
    !$omp master
    !$omp taskloop lastprivate (conditional: f) ! { dg-error "conditional 'lastprivate' clause on 'taskloop' construct" }
    do i = 1, 32
      if (p(i)) &
        f = i
    end do
!    !$omp master taskloop simd lastprivate (conditional: g) ! { dg!error "conditional 'lastprivate' clause on 'taskloop' construct" }
!    do i = 1, 32
!      if (p(i)) &
!        g = i
!    end do
    !$omp end master
  !$omp end parallel

!  !$omp parallel master taskloop simd lastprivate (conditional: h) ! { dg!error "conditional 'lastprivate' clause on 'taskloop' construct" }
!  do i = 1, 32
!    if (p(i)) &
!      h = i
!  end do
!  !$omp end parallel master taskloop simd
end subroutine

!struct S { int a, b; };

subroutine bar (p)
  implicit none
  logical :: p(:)
  type s_t
    integer :: a, b
  end type s_t
  type(s_t) s, t
  integer i
  s = s_t(-1, -1)
  t = s_t( 1, 2)
  !$omp parallel do lastprivate (conditional: s) ! { dg-error "non-scalar variable 's' in conditional 'lastprivate' clause" }
  do i = 1, 32
    if (p(i)) then
      block
       type(s_t) u
       u = t
       u%b = i
       s = u
      end block
    end if
  end do
end subroutine
