! { dg-do compile }
! { dg-options "-ffrontend-optimize -fdump-fortran-original" }
! PR fortran/52537 - optimize comparisons with empty strings
program main
  implicit none
  character(len=10) :: a
  character(len=30) :: line
  line = 'x'
  read (unit=line,fmt='(A)') a
  if (trim(a) == '') print *,"empty"
  call foo(a)
  if (trim(a) == '    ') print *,"empty"
contains
  subroutine foo(b)
    character(*) :: b
    if (b /= '   ') print *,"full"
  end subroutine foo
end program main
! { dg-final { scan-tree-dump-times "_gfortran_string_len_trim" 3 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
