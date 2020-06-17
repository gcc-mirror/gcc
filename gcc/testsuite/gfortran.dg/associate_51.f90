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
end

recursive function f3()
  associate (y1 => f3)
    print *, y1()  ! { dg-error "Expected array subscript" }
  end associate
  associate (y2 => f3) ! { dg-error "Associate-name 'y2' at \\(1\\) is used as array" }
    print *, y2(1)
  end associate
end

subroutine p2
  type t
  end type
  type(t) :: z = t()
  associate (y => t())
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
