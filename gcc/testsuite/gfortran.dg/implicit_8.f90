! { dg-do compile }
! PR 24748

! The compiler used to crash trying to take a substring of an implicit 
! real scalar.
subroutine variant1
 ybtable=ylocal(1:2)  ! { dg-error "Syntax error in argument list" }
end

! We want the behavior to be the same whether ylocal is implicitly 
! or explicitly typed.
subroutine variant2
 real ylocal
 ybtable=ylocal(1:2)  ! { dg-error "Syntax error in argument list" }
end

