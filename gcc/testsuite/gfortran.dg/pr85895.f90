! { dg-do compile }
! { dg-options "-fcoarray=lib" }
! PR fortran/85895
subroutine p
   character(80) :: c(2)
   sync memory (errmsg=c)        ! { dg-error "scalar CHARACTER variable" }
end subroutine p

subroutine q
   character(80) :: c(2)
   sync memory (errmsg=c(1:2))   ! { dg-error "scalar CHARACTER variable" }
end subroutine q

subroutine r
   character(80) :: c(2)
   sync memory (errmsg=c(1))
end subroutine r
