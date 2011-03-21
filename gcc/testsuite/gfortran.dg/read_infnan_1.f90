! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }

! PR43298  Fortran library does not read in NaN, NaN(), -Inf, or Inf

! Formatted READ part of PR fortran/43298

! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program pr43298
  real(4) :: x4(7)
  real(8) :: x8(7)
  character(80) :: output

open(10, status='scratch')
!                0123456789012345678901234567890123456789012345678901234567890123456789
write(10,'(a)') "inf           nan   infinity  NaN(dx)      -INf    NAN       InFiNiTy"
rewind(10)
x4 = 0.0_4
x8 = 0.0_8
read(10,'(7f10.3)') x4
rewind(10)
read(10,'(7f10.3)') x8
write (output, '("x4 =",7G6.0)') x4
if (output.ne."x4 =   Inf   NaN   Inf   NaN  -Inf   NaN   Inf") call abort
write (output, '("x8 =",7G6.0)') x8
if (output.ne."x8 =   Inf   NaN   Inf   NaN  -Inf   NaN   Inf") call abort
!print '("x4 =",7G6.0)', x4
!print '("x8 =",7G6.0)', x8
end program pr43298

