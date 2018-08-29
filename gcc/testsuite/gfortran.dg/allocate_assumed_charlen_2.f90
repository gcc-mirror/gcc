! { dg-do compile }
! PR fortran/82934
! PR fortran/83318
program a
 character(len=42), allocatable :: f
 character(len=22), allocatable :: ff
 call alloc(f, ff)
 if (len(f) .ne. 42) STOP 1
 if (len(ff) .ne. 22) STOP 2
contains
 subroutine alloc( a, b )
  character(len=*), allocatable  :: a
  character(len=22), allocatable :: b
  character(len=:), allocatable :: c
  character, allocatable :: d
  allocate(character(len=*)::a,b) ! { dg-error "Incompatible allocate-object" }
  allocate(character(len=*)::c)   ! { dg-error "Incompatible allocate-object" }
  allocate(character(len=*)::d)   ! { dg-error "Incompatible allocate-object" }
 end subroutine
end program a
