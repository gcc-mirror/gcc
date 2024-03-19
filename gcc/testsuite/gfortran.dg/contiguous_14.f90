! { dg-do compile }
! PR fortran/111503 - passing NULL() to POINTER, OPTIONAL, CONTIGUOUS dummy

program test
  implicit none
  integer, pointer, contiguous :: p(:) => null()
  integer, allocatable, target :: a(:)
  type t
     integer, pointer, contiguous :: p(:) => null()
     integer, allocatable         :: a(:)
  end type t
  type(t),               target :: z
  class(t), allocatable, target :: c
  print *, is_contiguous (p)
  allocate (t :: c)
  call one (p)
  call one ()
  call one (null ())
  call one (null (p))
  call one (a)
  call one (null (a))
  call one (z% p)
  call one (z% a)
  call one (null (z% p))
  call one (null (z% a))
  call one (c% p)
  call one (c% a)
  call one (null (c% p))
  call one (null (c% a))
contains
  subroutine one (x)
    integer, pointer, optional, contiguous, intent(in) :: x(:)
    print *, present (x)
    if (present (x)) then
       print *, "->", associated (x)
       if (associated (x)) stop 99
    end if
  end subroutine one
end
