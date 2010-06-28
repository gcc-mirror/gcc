! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
!
! List-directed part of PR fortran/43298
! and follow up to PR fortran/34319.
!
! Check handling of "NAN(alphanum)"
!
character(len=200) :: str
real :: r
complex :: z

! read_real:

r = 1.0
str = 'INfinity' ; read(str,*) r
if (r < 0 .or. r /= r*1.1) call abort()

r = 1.0
str = '-INF' ; read(str,*) r
if (r > 0 .or. r /= r*1.1) call abort()

r = 1.0
str = '+INF' ; read(str,*) r
if (r < 0 .or. r /= r*1.1) call abort()

r = 1.0
str = '-inFiniTY' ; read(str,*) r
if (r > 0 .or. r /= r*1.1) call abort()

r = 1.0
str = 'NAN' ; read(str,*) r
if (.not. isnan(r)) call abort()

r = 1.0
str = '-NAN' ; read(str,*) r
if (.not. isnan(r)) call abort()

r = 1.0
str = '+NAN' ; read(str,*) r
if (.not. isnan(r)) call abort()

r = 1.0
str = 'NAN(0x111)' ; read(str,*) r
if (.not. isnan(r)) call abort()

r = 1.0
str = '-NAN(123)' ; read(str,*) r
if (.not. isnan(r)) call abort()

r = 1.0
str = '+NAN(0xFFE)' ; read(str,*) r
if (.not. isnan(r)) call abort()


! parse_real

z = cmplx(-2.0,-4.0)
str = '(0.0,INfinity)' ; read(str,*) z
if (aimag(z) < 0 .or. aimag(z) /= aimag(z)*1.1) call abort()

z = cmplx(-2.0,-4.0)
str = '(-INF,0.0)' ; read(str,*) z
if (real(z) > 0 .or. real(z) /= real(z)*1.1) call abort()

z = cmplx(-2.0,-4.0)
str = '(0.0,+INF)' ; read(str,*) z
if (aimag(z) < 0 .or. aimag(z) /= aimag(z)*1.1) call abort()

z = cmplx(-2.0,-4.0)
str = '(-inFiniTY,0.0)' ; read(str,*) z
if (real(z) > 0 .or. real(z) /= real(z)*1.1) call abort()

z = cmplx(-2.0,-4.0)
str = '(NAN,0.0)' ; read(str,*) z
if (.not. isnan(real(z))) call abort()

z = cmplx(-2.0,-4.0)
str = '(0.0,-NAN)' ; read(str,*) z
if (.not. isnan(aimag(z))) call abort()

z = cmplx(-2.0,-4.0)
str = '(+NAN,0.0)' ; read(str,*) z
if (.not. isnan(real(z))) call abort()

z = cmplx(-2.0,-4.0)
str = '(NAN(0x111),0.0)' ; read(str,*) z
if (.not. isnan(real(z))) call abort()

z = cmplx(-2.0,-4.0)
str = '(0.0,-NaN(123))' ; read(str,*) z
if (.not. isnan(aimag(z))) call abort()

z = cmplx(-2.0,-4.0)
str = '(+nan(0xFFE),0.0)' ; read(str,*) z
if (.not. isnan(real(z))) call abort()

end
