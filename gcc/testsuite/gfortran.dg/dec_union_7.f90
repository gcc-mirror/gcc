! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! Comprehensive compile tests for what unions CAN'T do.
!

! Syntax errors
structure /s0/
  union a b c      ! { dg-error "Junk after UNION" }
  union
    map a b c      ! { dg-error "Junk after MAP" }
    integer x      ! { dg-error "Unexpected" }
    structure /s2/ ! { dg-error "Unexpected" }
    map
      map          ! { dg-error "Unexpected" }
    end map
  end union
end structure

! Initialization expressions
structure /s1/
  union
    map
      integer(4) :: x = 1600 ! { dg-error "Conflicting initializers" }
      integer(4) :: y = 1800
    end map
    map
      integer(2) a, b, c, d
      integer :: e = 0 ! { dg-error "Conflicting initializers" }
    end map
    map
      real :: p = 1.3, q = 3.7 ! { dg-error "Conflicting initializers" }
    end map
  end union
end structure
record /s1/ r1

end
