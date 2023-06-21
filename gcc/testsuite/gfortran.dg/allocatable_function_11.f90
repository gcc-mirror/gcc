! { dg-do compile }
! PR fortran/109500 - check F2018:8.5.3 Note 1
!
! The result of referencing a function whose result variable has the
! ALLOCATABLE attribute is a value that does not itself have the
! ALLOCATABLE attribute.

program main
  implicit none
  integer, allocatable  :: p
  procedure(f), pointer :: pp
  pp => f
  p = f()
  print *, allocated (p)
  print *, is_allocated (p)
  print *, is_allocated (f())  ! { dg-error "is a function result" }
  print *, is_allocated (pp()) ! { dg-error "is a function result" }
  call s (p)
  call s (f())  ! { dg-error "is a function result" }
  call s (pp()) ! { dg-error "is a function result" }

contains
  subroutine s(p)
    integer, allocatable :: p
  end subroutine s

  function f()
    integer, allocatable :: f
    allocate (f, source=42)
  end function

  logical function is_allocated(p)
    integer, allocatable :: p
    is_allocated = allocated(p)
  end function
end program
