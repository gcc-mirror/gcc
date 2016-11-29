! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! PR fortran/78277
!
! Fix ICE for invalid structure declaration code.
!

subroutine sub1()
  structure /s/
    structure t
      integer i
    end structure
  end structure
  record /s/ u
  interface
    subroutine sub0(u)
      structure /s/
        structure t. ! { dg-error "Syntax error in anonymous structure decl" }
          integer i
        end structure
      end structure
      record /s/ u
    end
  end interface
  call sub0(u)
end
