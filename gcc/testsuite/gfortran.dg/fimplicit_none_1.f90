! { dg-do compile }
! { dg-options "-fimplicit-none" }
subroutine s(n) ! { dg-error "has no IMPLICIT type" }
   character(n) :: c  ! { dg-error "Scalar INTEGER expression expected" }
   c = 'c' ! { dg-error "has no IMPLICIT type" }
end
