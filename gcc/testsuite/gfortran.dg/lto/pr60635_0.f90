! { dg-lto-do link }
! { dg-lto-options {{ -Wno-lto-type-mismatch }} }
program test
  use iso_fortran_env

  interface
    integer(int16) function bigendc16(x) bind(C)
      import
      integer(int16), intent(in) :: x
    end function
  end interface
  
  integer(int16) :: x16 = 12345
  x16 = bigendc16(x16)
  print *,x16
end program

