! { dg-do compile }
! PR fortran/71884 - reject NULL as source-expr in ALLOCATE with SOURCE= or MOLD=
!
! Contributed by G.Steinmetz

program p
  real,     allocatable :: a
  real,     pointer     :: b
  class(*), allocatable :: x
  class(*), pointer     :: y

  allocate (x, source=null())  ! { dg-error "NULL cannot be used as source-expr" }
  allocate (y, source=null(b)) ! { dg-error "NULL cannot be used as source-expr" }
  allocate (x, mold=null(b))   ! { dg-error "NULL cannot be used as source-expr" }
  allocate (y, mold=null())    ! { dg-error "NULL cannot be used as source-expr" }
end
