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

  deallocate(i, stat=x) ! { dg-error "must be a scalar INTEGER" }
  deallocate(i, stat=j, stat=k) ! { dg-error "Redundant STAT" }
  deallocate(i)
  deallocate(i)) ! { dg-error "Syntax error in DEALLOCATE" }
  deallocate(i, errmsg=err, errmsg=err) ! { dg-error "Redundant ERRMSG" }
  deallocate(i, errmsg=err) ! { dg-warning "useless without a STAT" }
  deallocate(i, stat=j, errmsg=x) ! { dg-error "must be a scalar CHARACTER" }

  deallocate(err) ! { dg-error "nonprocedure pointer or an allocatable" }

  deallocate(error,stat=j,errmsg=error(1)) ! { dg-error "shall not be DEALLOCATEd within" }
  deallocate(i, stat = i(1))  ! { dg-error "shall not be DEALLOCATEd within" }

  deallocate(n) ! { dg-error "must be ALLOCATABLE or a POINTER" }

  deallocate(i, i) ! { dg-error "Allocate-object at" }

  ! These should not fail the check for duplicate alloc-objects.
  deallocate(f(1)%c, f(2)%d)
  deallocate(e%c, e%d)

end program a
