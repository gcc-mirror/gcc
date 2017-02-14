! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! Verify that the comparisons in gfc_compare_derived_types can correctly
! match nested anonymous subtypes.
!

subroutine sub0 (u)
  structure /t/
    structure sub
      integer i
    end structure
  endstructure
  record /t/ u
  u.sub.i = 0
end subroutine sub0

subroutine sub1 ()
  structure /t/
    structure sub
      integer i
    end structure
  endstructure
  record /t/ u

  interface
    subroutine sub0 (u) ! regression: Interface mismatch.*Type mismatch
      structure /t/
        structure sub
          integer i
        end structure
      endstructure
        record /t/ u
    end subroutine
  end interface

  call sub0(u) ! regression: Type mismatch in argument
end subroutine

subroutine sub2(u)
  structure /tu/
    union
      map
        integer i
      end map
      map
        real r
      end map
    end union
  end structure
  record /tu/ u
  u.r = 1.0
end subroutine

implicit none

structure /t/
  structure sub
    integer i
  end structure
endstructure

structure /tu/
  union
    map
      integer i
    end map
    map
      real r
    end map
  end union
end structure

record /t/ u
record /tu/ u2

call sub0(u) ! regression: Type mismatch in argument
call sub1()
call sub2(u2)

end
