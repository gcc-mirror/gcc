! { dg-do compile }
! PR fortran/77414
subroutine a(x)               ! { dg-error "(1)" }
   character(*) :: x
   contains
      subroutine a(x)         ! { dg-error " is already defined at" }
         character(*) :: x    ! { dg-error "Unexpected data declaration statement in CONTAINS section" }
      end subroutine a
end subroutine a  ! { dg-error "Expecting END PROGRAM statement" }
