! { dg-do compile }
program main
  type t1
     integer, allocatable :: x(:)
     integer, allocatable :: y(:)
  end type t1
  type(t1), allocatable :: v(:)
  allocate (v(3), v(4))  ! { dg-error "Allocate-object at \\(1\\) also appears at \\(2\\)" }
  allocate (v(1), v(1)%x(2)) ! { dg-error "Allocate-object at \\(1\\) is subobject of object at \\(2\\)" }
  allocate (v(1)%x(2), v(1)) ! { dg-error "Allocate-object at \\(1\\) is subobject of object at \\(2\\)" }
  allocate (v(1)%y(2), v(1)%x(1))
  allocate (v(2)%x(3), v(2)%x(3)) ! { dg-error "Allocate-object at \\(1\\) also appears at \\(2\\)" }
  allocate (v(1)%x(3), v(2)%x(3))
  deallocate (v, v)  ! { dg-error "Allocate-object at \\(1\\) also appears at \\(2\\)" }
  deallocate (v, v(1)%x) ! { dg-error "Allocate-object at \\(1\\) is subobject of object at \\(2\\)" }
  deallocate (v(1)%x, v) ! { dg-error "Allocate-object at \\(1\\) is subobject of object at \\(2\\)" }
  deallocate (v(1)%y, v(1)%x)
  deallocate (v(2)%x, v(2)%x) ! { dg-error "Allocate-object at \\(1\\) also appears at \\(2\\)" }
  deallocate (v(1)%x, v(2)%x)
end program main
