! { dg-do compile }
! PR fortran/100274 - ICE in gfc_conv_procedure_call, at fortran/trans-expr.c:6131

program p
  call s('y')   ! { dg-warning "Character length of actual argument" }
contains
  subroutine s(x)
    character(8), intent(out) :: x
  end
end

! { dg-error "in variable definition context"  " " { target *-*-* } 5 }
