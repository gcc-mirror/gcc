! { dg-do compile }
! { dg-require-effective-target lto }
! { dg-options "-flto" }
! PR 78867, test case adapted from gfortran.dg/string_length_1.f90
program pr78867
  if (len(bar(2_8)) /= 2) call abort
contains

  function bar(i)
    integer(8), intent(in) :: i
    character(len=i) :: bar
  
    bar = ""
  end function bar

end program pr78867
