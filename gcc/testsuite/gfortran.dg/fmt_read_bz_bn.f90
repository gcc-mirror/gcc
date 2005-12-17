! { dg-do run }
! Test various uses of BZ and BN format specifiers.
! Portions inspired by NIST F77 testsuite FM711.f
! Contributed by jvdelisle@verizon.net
program test_bn
        
integer I1(2,2), I2(2,2,2)
real A1(5)
real(kind=8) A2(0:3)
character*80 :: IDATA1="111 2 2 3 3. 3E-1  44 5 5 6 . 67 . 78 8. 8E-1"
character*80 :: IDATA2="2345 1 34512 45123 51234 2345 1 34512 45123 5"
character*80 :: IDATA3="-8.0D0  1.0D-4  0.50D0  0.250D0"
character*80 :: ODATA=""
character*80 :: CORRECT1=" 1110 2020 .30303E-07   44   55   6.6 70.07 .888E+01"
character*80 :: CORRECT2="23450 10345. 12.45 1235 1234 2345  1345. 12.45 1235"
character*80 :: CORRECT3="   -0.8000000000D+01    0.1000000000D-03&
    0.5000000000D+00    0.2500000000D+00"
READ(IDATA1, 10) I1(1,2), IVI, A1(3), JVI, KVI, A1(2), AVS, A1(1)
10 FORMAT (BZ,(2I4, E10.1, BN, 2I4, F5.2, BZ, F5.2, BN, E10.1))

WRITE(ODATA, 20) I1(1,2), IVI, A1(3), JVI, KVI, A1(2), AVS, A1(1)
20 FORMAT (2I5, 1X, E10.5, BN, 2I5, F6.1, BZ, F6.2, BN, 1X, E8.3, I5)

if (ODATA /= CORRECT1) call abort
ODATA=""

READ(IDATA2, 30) I2(1,2,1), A1(3), AVS, IVI, I1(1,1), JVI, BVS, A1(2), I2(1,1,1)
30 FORMAT (BZ, (I5, F5.0, BN, F5.2, 2I5, I5, F5.0, BN, F5.2, I5))

WRITE(ODATA, 40) I2(1,2,1), A1(3), AVS, IVI, I1(1,1), JVI, BVS, A1(2), I2(1,1,1)
40 FORMAT (I5, F7.0, BZ, 1X, F5.2, 2(1X,I4),I5, F7.0, BZ, 1X, F5.2, 1X, I4)

if (ODATA /= CORRECT2) call abort
ODATA=""

READ(IDATA3, 50) A2
50 FORMAT (4D8.0)

WRITE(ODATA,60) A2
60 FORMAT (4D20.10)

if (ODATA /= CORRECT3) call abort

end program test_bn
