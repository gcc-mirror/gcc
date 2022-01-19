! PR middle-end/102860
! { dg-do compile { target { powerpc*-*-* } } }
! { dg-require-effective-target powerpc_vsx_ok } 
! { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power10" } } 
! { dg-options "-O2 -mcpu=power10" } 

function foo(a)
  integer(kind=4) :: a(1024)
  a(:) = modulo (a(:), 39)
end function
