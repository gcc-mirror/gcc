! PR middle-end/102860
! { dg-do compile { target { powerpc*-*-* } } }
! { dg-require-effective-target powerpc_vsx_ok } 
! { dg-options "-O2 -mdejagnu-cpu=power10" } 

function foo(a)
  integer(kind=4) :: a(1024)
  a(:) = modulo (a(:), 39)
end function
