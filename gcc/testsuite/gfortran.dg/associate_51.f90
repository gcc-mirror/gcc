! { dg-do compile }
!
! PR fortran/92994
!
! Contributed by G. Steinmetz
!
recursive function f() result(z)
  associate (y1 => f())
  end associate
  associate (y2 => f)  ! { dg-error "is a procedure name" }
  end associate
end

recursive function f2()
  associate (y1 => f2()) ! { dg-error "Invalid association target" }
  end associate          ! { dg-error "Expecting END FUNCTION statement" }
  associate (y2 => f2)   ! { dg-error "is a procedure name" }
  end associate
end

subroutine p2
  type t
  end type
  type(t) :: z = t()
  associate (y => t)
  end associate
end

subroutine p3
  procedure() :: g
  associate (y => g)  ! { dg-error "is a procedure name" }
  end associate
end

subroutine p4
  external :: g
  associate (y => g)  ! { dg-error "is a procedure name" }
  end associate
end

recursive subroutine s
  associate (y => s)  ! { dg-error "is a procedure name" }
  end associate
end

recursive subroutine s2
   associate (y => (s2)) ! { dg-error "Associating selector-expression at .1. yields a procedure" }
   end associate
end

program p
   associate (y => (p)) ! { dg-error "Invalid association target" }
   end associate ! { dg-error "Expecting END PROGRAM statement" }
end
