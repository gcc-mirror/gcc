! { dg-do compile }
! PR fortran/77414
subroutine a(x)               ! { dg-error "(1)" }
   character(*) :: x
   contains
      subroutine a(x)         ! { dg-error " is already defined at" }
         character(*) :: x
      end subroutine a
end subroutine a
