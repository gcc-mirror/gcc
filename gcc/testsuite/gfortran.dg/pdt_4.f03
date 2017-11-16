! { dg-do compile }
!
! Test bad PDT coding: Based on pdt_3.f03
!
module m
  integer :: d_dim = 4
  integer :: mat_dim = 256
  integer, parameter :: ftype = kind(0.0d0)
  type :: modtype (a,b)
    integer, kind :: a = kind(0.0e0)
    integer, LEN :: b = 4
    integer :: i
    real(kind = a) :: d(b, b)
  end type
end module

module bad_vars
  use m
  type(modtype(8,mat_dim)) :: mod_q ! { dg-error "must not have the SAVE attribute" }
  type(modtype(8,*)) :: mod_r       ! { dg-error "ASSUMED type parameters" }
end module

  use m
  implicit none
  integer :: i
  integer, kind :: bad_kind    ! { dg-error "not allowed outside a TYPE definition" }
  integer, len :: bad_len      ! { dg-error "not allowed outside a TYPE definition" }

  type :: bad_pdt (a,b, c, d)  ! { dg-error "does not have a component" }
    real, kind :: a            ! { dg-error "must be INTEGER" }
    INTEGER(8), kind :: b      ! { dg-error "be default integer kind" }
    real, LEN :: c             ! { dg-error "must be INTEGER" }
    INTEGER(8), LEN :: d       ! { dg-error "be default integer kind" }
  end type

  type :: mytype (a,b)
    integer, kind :: a = kind(0.0e0)
    integer, LEN :: b = 4
    integer :: i
    real(kind = a) :: d(b, b)
  end type

  type, extends(mytype) :: thytype(h)
    integer, kind :: h
    integer(kind = h) :: j
  end type

  type x (q, r, s)
    integer, kind :: q
    integer, kind :: r
    integer, LEN :: s
    integer(kind = q) :: idx_mat(2,2)
    type (mytype (b=s)) :: mat1
    type (mytype (b=s*2)) :: mat2
  end type x

  real, allocatable :: matrix (:,:)

! Bad KIND parameters
  type(thytype(d_dim, 4, 4)) :: wbad ! { dg-error "does not reduce to a constant" }
  type(thytype(*, 4, 4)) :: worse    ! { dg-error "cannot either be ASSUMED or DEFERRED" }
  type(thytype(:, 4, 4)) :: w_ugh    ! { dg-error "cannot either be ASSUMED or DEFERRED" }

  type(thytype(ftype, b=4, h=4)) :: w
  type(x(8,4,mat_dim)) :: q          ! { dg-error "must not have the SAVE attribute" }
  class(mytype(ftype, :)), allocatable :: cz

  w%a = 1                           ! { dg-error "assignment to a KIND or LEN component" }
  w%b = 2                           ! { dg-error "assignment to a KIND or LEN component" }
  w%h = 3                           ! { dg-error "assignment to a KIND or LEN component" }

  w%d = reshape ([(real(i), i = 1, d_dim*d_dim)],[d_dim,d_dim])

  matrix = w%d

  allocate (cz, source = mytype(*, d_dim, 0, matrix)) ! { dg-error "Syntax error" }
  allocate (cz, source = mytype(ftype, :, 0, matrix)) ! { dg-error "Syntax error" }
  select type (cz)
    type is (mytype(ftype, d_dim))  ! { dg-error "must be ASSUMED" }
      if (int (sum (cz%d)) .ne. 136) call abort ! { dg-error "Expected TYPE IS" }
    type is (thytype(ftype, *, 8))
      call abort
  end select
  deallocate (cz)

  allocate (thytype(ftype, d_dim*2, 8) :: cz)
  cz%d = reshape ([(i*10, i = 1, cz%b**2)], [cz%b,cz%b])
  select type (cz)
    type is (mytype(4, *))        !  { dg-error "must be an extension" }
      call abort
    type is (thytype(ftype, *, 8))
      if (int (sum (cz%d)) .ne. 20800) call abort
  end select
  deallocate (cz)
contains
  subroutine foo(arg)
    type (mytype(4, *)) :: arg      ! OK
  end subroutine
  subroutine bar(arg)               ! OK
    type (thytype(8, :, 4) :: arg
  end subroutine
end
