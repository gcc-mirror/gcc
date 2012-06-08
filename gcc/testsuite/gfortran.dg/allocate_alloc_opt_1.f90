! { dg-do compile }
program a

  implicit none

  real x
  integer j, k, n(4)
  character(len=70) err
  character(len=70), allocatable :: error(:)

  integer, allocatable :: i(:)

  type b
    integer, allocatable :: c(:), d(:)
  end type b

  type(b) e, f(3)

  allocate(i(2), stat=x) ! { dg-error "must be a scalar INTEGER" }
  allocate(i(2), stat=j, stat=k) ! { dg-error "Redundant STAT" }
  allocate(i(2))
  allocate(i(2))) ! { dg-error "Syntax error in ALLOCATE" }
  allocate(i(2), errmsg=err, errmsg=err) ! { dg-error "Redundant ERRMSG" }
  allocate(i(2), errmsg=err) ! { dg-warning "useless without a STAT" }
  allocate(i(2), stat=j, errmsg=x) ! { dg-error "must be a scalar CHARACTER" }

  allocate(err) ! { dg-error "neither a data pointer nor an allocatable" }

  allocate(error(2),stat=j,errmsg=error(1)) ! { dg-error "shall not be ALLOCATEd within" }
  allocate(i(2), stat = i(1))  ! { dg-error "shall not be ALLOCATEd within" }

  allocate(n) ! { dg-error "must be ALLOCATABLE or a POINTER" }

  allocate(i(2), i(2)) ! { dg-error "Allocate-object at" }

  ! These should not fail the check for duplicate alloc-objects.
  allocate(f(1)%c(2), f(2)%d(2))
  allocate(e%c(2), e%d(2))

end program a
