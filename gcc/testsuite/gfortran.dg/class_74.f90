! { dg-do compile }
! { dg-additional-options "-fcoarray=single" }
!
! PR fortran/106856
!
! Contributed by G. Steinmetz 
!
subroutine foo
  interface
    subroutine bar(x)
      type(*) :: x
    end subroutine bar
  end interface
  class(*) :: x, y
  allocatable :: x
  dimension :: x(:), y(:,:)
  codimension :: x[:]
  pointer :: y
  y => null()
  if (allocated(x)) then
    call bar(x(2)[1])
  end if
  if (associated(y)) then
    call bar(y(2,2))
  end if
end subroutine foo


program p
  class(*), allocatable :: x, y
  y = 'abc'
  call s1(x, y)
contains
  subroutine s1(x, y)
    class(*) :: x, y
  end
  subroutine s2(x, y)
    class(*), allocatable :: x, y
    optional :: x
  end
end


subroutine s1 (x)
  class(*)    :: x
  allocatable :: x
  dimension   :: x(:)
  if (allocated (x)) print *, size (x)
end

subroutine s2 (x)
  class(*)    :: x
  allocatable :: x(:)
  if (allocated (x)) print *, size (x)
end

subroutine s3 (x)
  class(*)    :: x(:)
  allocatable :: x
  if (allocated (x)) print *, size (x)
end

subroutine s4 (x)
  class(*)    :: x
  dimension   :: x(:)
  allocatable :: x
  if (allocated (x)) print *, size (x)
end


subroutine c0 (x)
  class(*)    :: x
  allocatable :: x
  codimension :: x[:]
  dimension   :: x(:)
  if (allocated (x)) print *, size (x)
end

subroutine c1 (x)
  class(*)    :: x(:)
  allocatable :: x[:]
  if (allocated (x)) print *, size (x)
end

subroutine c2 (x)
  class(*)    :: x[:]
  allocatable :: x(:)
  if (allocated (x)) print *, size (x)
end

subroutine c3 (x)
  class(*)    :: x(:)[:]
  allocatable :: x
  if (allocated (x)) print *, size (x)
end

subroutine c4 (x)
  class(*)    :: x
  dimension   :: x(:)
  codimension :: x[:]
  allocatable :: x
  if (allocated (x)) print *, size (x)
end


subroutine p1 (x)
  class(*)    :: x
  pointer     :: x
  dimension   :: x(:)
  if (associated (x)) print *, size (x)
end

subroutine p2 (x)
  class(*)    :: x
  pointer     :: x(:)
  if (associated (x)) print *, size (x)
end

subroutine p3 (x)
  class(*)    :: x(:)
  pointer     :: x
  if (associated (x)) print *, size (x)
end

subroutine p4 (x)
  class(*)    :: x
  dimension   :: x(:)
  pointer     :: x
  if (associated (x)) print *, size (x)
end


! Testcase by Mikael Morin
subroutine mm ()
  pointer   :: y
  dimension :: y(:,:)
  class(*)  :: y
  if (associated (y)) print *, size (y)
end

! Testcase from pr53951
subroutine pr53951 ()
  type t
  end type t
  class(t), pointer :: C
  TARGET :: A
  class(t), allocatable :: A, B
  TARGET :: B
  C => A ! Valid
  C => B ! Valid, but was rejected
end
