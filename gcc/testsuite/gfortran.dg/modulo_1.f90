! { dg-do compile }
! PR fortran/23912
  integer*4 i4
  integer*8 i8

  i4 = modulo(i4,i8) ! { dg-warning "Extension" }
  i4 = modulo(i8,i4) ! { dg-warning "Extension" }

  end
