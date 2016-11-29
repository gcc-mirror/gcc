! { dg-do "compile" }
! { dg-options "-fdec-structure" }
!
! PR fortran/78259
!
! ICE in gfc_trans_subcomponent_assign
!

subroutine sub
  structure /s/
    union
      map
        integer n(2)
      end map
      map
        integer(8) m /2/
      end map
    end union
  end structure
  record /s/ r
  r.n(1) = 1
end
