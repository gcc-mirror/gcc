! { dg-do run }
! { dg-options "-fdec-structure" }
!
! sub0 and sub1 test a regression where calling gfc_use_derived from
! gfc_find_component on the structure type symbol being parsed caused the
! symbol to be freed and swapped for the previously seen type symbol, leaving
! dangling pointers and causing all sorts of mayhem.
!

subroutine sub0 (u)
  structure /s/
    union ! U0
      map ! M0
        integer i
      end map
    end union
  end structure
  record /s/ u
  u.i = 0
end subroutine sub0

subroutine sub1 ()
  structure /s/
    union ! U1
      map ! M1
        integer i
      end map
    end union
  end structure
  record /s/ u
  interface ! matches the declaration of sub0 above
    subroutine sub0 (u)
      structure /s/
        union ! U2
          map ! M2
            integer i ! gfc_find_component should not call gfc_use_derived
          end map     ! here, otherwise this structure's type symbol is freed
        end union     ! out from under it
      end structure
      record /s/ u
    end subroutine sub0
  end interface
  call sub0(u)
end subroutine

! If sub0 and sub1 aren't used they may be omitted
structure /s/
  union ! U1
    map ! M3
      integer i
    end map
  end union
end structure
record /s/ u

call sub0(u)
call sub1()

end
