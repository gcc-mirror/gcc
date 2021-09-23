! { dg-do compile }
! { dg-options "-fcheck=bounds" }
! PR fortran/100656 - ICE in gfc_conv_expr_present

subroutine s(x)
  character(:), allocatable, optional :: x(:)
  if ( present(x) ) then
     if ( allocated(x) ) then
        x = 'a' // x // 'e'
     end if
  end if
end
