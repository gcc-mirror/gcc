! { dg-do compile }
! PR fortran/101564 - ICE in resolve_allocate_deallocate

program p
  implicit none
  integer, allocatable :: x(:)
  integer              :: stat
  integer, pointer     :: A
  integer, target      :: ptr
  real,    target      :: r
  character(80)        :: c
  type t
     integer :: stat
     real    :: r
     complex :: z
  end type t
  type(t), allocatable :: y
  type tc
     character(len=:), allocatable :: s
  end type tc
  type(tc) :: z
  allocate (character(42) :: z%s, stat=stat)
  allocate (x(2), stat=stat)
  deallocate (x,  stat=stat)
  allocate (A,    stat=f())
  deallocate (A,  stat=f())
  allocate (A,    stat=y%stat)
  deallocate (A,  stat=y%stat)
  allocate (A,    stat=stat, errmsg=c(2:79))
  deallocate (A,  stat=stat, errmsg=c(2:79))
  allocate (A,    stat=stat, errmsg=z%s)
  deallocate (A,  stat=stat, errmsg=z%s)
  allocate (A,    stat=stat, errmsg=z%s(2:39))
  deallocate (A,  stat=stat, errmsg=z%s(2:39))
  allocate (A,    stat=y%r)  ! { dg-error "must be a scalar INTEGER variable" }
  deallocate (A,  stat=y%r)  ! { dg-error "must be a scalar INTEGER variable" }
  allocate (x(2), stat=stat%kind) ! { dg-error "STAT tag" }
  deallocate (x,  stat=stat%kind) ! { dg-error "STAT variable" }
  allocate (A,    stat=A%kind)    ! { dg-error "STAT tag" }
  deallocate (A,  stat=A%kind)    ! { dg-error "STAT variable" }
  allocate (A,    stat=c%len)     ! { dg-error "STAT tag" }
  deallocate (A,  stat=c%len)     ! { dg-error "STAT variable" }
  allocate (A,    stat=y%stat%kind) ! { dg-error "STAT tag" }
  deallocate (A,  stat=y%stat%kind) ! { dg-error "STAT variable" }
  allocate (y, stat=y%stat) ! { dg-error "within the same ALLOCATE statement" }
  allocate (y, stat=r)      ! { dg-error "must be a scalar INTEGER variable" }
  allocate (A, stat=y%z%re)   ! { dg-error "must be a scalar INTEGER variable" }
  deallocate (A, stat=y%z%im) ! { dg-error "must be a scalar INTEGER variable" }
  allocate (y, stat=g())    ! { dg-error "must be a scalar INTEGER variable" }
  deallocate (y, stat=g())  ! { dg-error "must be a scalar INTEGER variable" }
  allocate (A, stat=f)      ! { dg-error "requires an argument list" }
  deallocate (A, stat=f)    ! { dg-error "requires an argument list" }
  allocate (y, stat=g)      ! { dg-error "requires an argument list" }
  deallocate (y, stat=g)    ! { dg-error "requires an argument list" }
  allocate (A, stat=z%s%len)   ! { dg-error "parameter inquiry" }
  deallocate (A, stat=z%s%len) ! { dg-error "parameter inquiry" }
  allocate (A,   stat=f(), errmsg="") ! { dg-error "ERRMSG variable" }
  deallocate (A, stat=f(), errmsg="") ! { dg-error "ERRMSG variable" }
  allocate (A,   stat=stat, errmsg=z%s%len) ! { dg-error "ERRMSG variable" }
  deallocate (A, stat=stat, errmsg=z%s%len) ! { dg-error "ERRMSG variable" }
  deallocate (z%s, stat=stat, errmsg=z%s)   ! { dg-error "within the same DEALLOCATE statement" }
contains
  integer function f()
    pointer :: f
    f => ptr
  end function f
  real function g()
    pointer :: g
    g => r
  end function g
end
