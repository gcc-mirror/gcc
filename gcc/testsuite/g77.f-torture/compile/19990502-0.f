      SUBROUTINE TRF2F2(QDERIV,QPRINT,
     @                  XRH,XRK,XRL,FCALC,FOBS,FPART,WEIGHT,TEST,FOM,
     @                  ITEST)
C
C Computes the standard linear correlation coefficient between
C  F(obs)^2  and F(calc)^2 or between |F(obs)|  and |F(calc)|
C
C Author: Axel T. Brunger
C =======================
      IMPLICIT NONE
C I/O
C*
C*    BEGINNING OF INCLUDE FILE: xrefin.fcm
C*
C
C XREFIN.FCM
C
C data structure for XREFIN.FLX
C crystallographic restraints
C
C update flags
      LOGICAL XRQCHK, XRUPAT, XRREUP
C
C method flag
      LOGICAL QFFT, QLOOK
C target function string
      CHARACTER*4 XRTRGT
C
C tolerance for linear approximation
      DOUBLE PRECISION XRLTOL
C
C x-ray diffraction data
C XRMREF: max. allocation for reflections
C XRNREF: current number of reflections
C XRIREF: number of reflections within limits (resolution, f_window...)
C XRNPHA: number of phase specifications
C XRH, XRK, XRL: reflection indices
C FOBS: observed structure factor
C FOM: figure of merit for observed phases (zero if not used)
C WEIGHT: individual weight
C FCALC: calculated structure factor
C FPART: partial structure factor to be added to FCALC
C TEST: integer array for cross-validation tests
      INTEGER XRMREF, XRNREF, XRIREF, XRNPHA
      INTEGER HPH, HPK, HPL, HPFOBS, HPFCAL, HPFPAR, HPFOM
      INTEGER HPWEIG, HPTEST, HPSIGM
C scattering tables
      INTEGER XRSM, XRSN
      PARAMETER (XRSM=20)
      DOUBLE PRECISION XRSA(XRSM,4), XRSB(XRSM,4), XRSC(XRSM)
      DOUBLE PRECISION XRF(XRSM), XRSI(XRSM)
C unit cell
      DOUBLE PRECISION XRCELL(9), XRTR(3,3), XRINTR(3,3), XRVOL
C symmetry operators
      INTEGER XRNSYM, XRMSYM, XRSYTH
      PARAMETER (XRMSYM=192, XRSYTH=24)
      INTEGER XRSYMM(XRMSYM,3,4), XRITSY(XRMSYM,3,3)
      LOGICAL QHERM
C reciprocal resolution limits
      DOUBLE PRECISION XRHIGH, XRLOW
C fobs limits
      DOUBLE PRECISION XRFLOW, XRFHIG
C XREFIN atom lists
      INTEGER XRMATO, XRNATO, XRNATF, HPFLAG, HPATOM, HPINDX
      INTEGER HPATOF, HPINDF, HPFX, HPFY, HPFZ, HPFB, HPFQ, HPFQS
      INTEGER HPDX, HPDY, HPDZ, HPDT, HPDQ
C scale factor
      DOUBLE PRECISION XRSCAL
C phase potential scale factor and exponent
      DOUBLE PRECISION XRPSCA
      INTEGER XRPEXP
C Fobs/Fcalc scale factor
      DOUBLE PRECISION XRFFK
      LOGICAL XRFFKQ
C unscaled restraint energies
      DOUBLE PRECISION XRE, XREPHA
C number of bins for R factor analysis
      INTEGER MBINS
C logical flag indicating the presence of TEST sets (for
C cross-validation)
      LOGICAL XCVTEST
C
C     double precision common block
C
      COMMON /XREFI/ XRLTOL,
     @               XRSA, XRSB, XRSC, XRF, XRSI,
     @               XRCELL, XRTR, XRINTR, XRHIGH, XRLOW,
     @               XRSCAL, XRPSCA,
     @               XRFFK,  XRE, XREPHA,
     @               XRFLOW, XRFHIG, XRVOL
C
C     integer common block
C
      COMMON /IXREFI/
     @               XRMREF, XRNREF, XRIREF, XRNPHA, HPH, HPK, HPL,
     @               HPFOBS, HPFCAL, HPFPAR, HPFOM, HPWEIG, HPTEST,
     @               HPSIGM, XRSN,   HPFLAG,
     @               XRMATO, XRNATO, HPATOM, HPINDX, XRNATF, HPATOF,
     @               HPINDF, HPFX, HPFY, HPFZ, HPFB, HPFQ, HPFQS,
     @               HPDX, HPDY, HPDZ, HPDT, HPDQ,
     @               XRPEXP,
     @               XRNSYM, XRSYMM, MBINS, XRITSY
C
C     logical common block
C
      COMMON /LXREFI/ XRQCHK,  XRUPAT,  XRFFKQ,
     @                QFFT,    QLOOK,  XRREUP, QHERM, XCVTEST
C
C     character string common block
C
      COMMON /CXREFI/ XRTRGT
C
      SAVE /XREFI/
      SAVE /IXREFI/
      SAVE /LXREFI/
      SAVE /CXREFI/
C*
C*    BEGINNING OF INCLUDE FILE: consta.fcm
C*
C  CONSTA.FCM
C
C this file contains all physical and mathematical constants
C and conversion factors.
C
C at present the following units are used:
C
C   length: Angstroms
C   time: ps
C   energy: Kcal/mol
C   mass: atomic-mass-unit
C   charge: electron-charge
C
C
      DOUBLE PRECISION RSMALL
      PARAMETER (RSMALL=1.0D-10)
      DOUBLE PRECISION R4SMAL,R4BIG
      PARAMETER (R4SMAL=0.0001D0,R4BIG=1.0D+10)
C
C physical constants in SI units
C ------------------------------
C     Kb = 1.380662 E-23 J/K
C     Na = 6.022045 E23  1/mol
C     e = 1.6021892 E-19 C
C     eps = 8.85418782 E-12 F/m
C
C     1 Kcal = 4184.0 J
C     1 amu = 1.6605655 E-27 Kg
C     1 A = 1.0 E-10 m
C
C reference: CRC Handbook for Chemistry and Physics, 1983/84
C
C
      DOUBLE PRECISION PI
      PARAMETER(PI=3.1415926535898D0)
C
C     TIMFAC is the conversion factor from AKMA time to picoseconds.
C            (TIMFAC = SQRT ( ( 1A )**2 * 1amu * Na  / 1Kcal )
C            this factor is used only intrinsically, all I/O is in ps.
C
      DOUBLE PRECISION TIMFAC
      PARAMETER (TIMFAC=0.04888821D0)
C
C KBOLTZ is Boltzman constant AKMA units (KBOLTZ = N *K  / 1 Kcal)
C                                                   a  b
      DOUBLE PRECISION KBOLTZ
      PARAMETER (KBOLTZ=1.987191D-03)
C
C CCELEC is 1/ (4 pi eps ) in AKMA units, conversion from SI
C units: CCELEC = e*e*Na / (4*pi*eps*1Kcal*1A)
C
      DOUBLE PRECISION CCELEC
      PARAMETER (CCELEC=332.0636D0)
C
C CDEBHU is used in the Debye-Hueckel approximation:
C      DIV GRAD phi = kappa**2 phi
C      kappa**2 = CDEBHU * ionic_strength [M] / ( T [K] eps   )
C                                                          ext
C where CDEBHU is defined as CDEBHU=2E+3 Na e**2 / (eps0 Kb )
C (in SI units, ref: Gordon M.Barrow, Physical Chemistry,
C McGraw Hill (1979) ) and ionic_strength is given in molar units.
C The conversion to AKMA units brings another factor 1.0E-20.
C
      DOUBLE PRECISION CDEBHU
      PARAMETER (CDEBHU=2529.09702D0)
      LOGICAL QDERIV, QPRINT
      INTEGER XRH(*), XRK(*), XRL(*)
      DOUBLE COMPLEX FCALC(*), FOBS(*), FPART(*)
      DOUBLE PRECISION WEIGHT(*)
      INTEGER TEST(*)
      DOUBLE PRECISION FOM(*)
      INTEGER ITEST
C local
      INTEGER REFLCT
      DOUBLE PRECISION CI, CJ, CII, CJJ, CIJ, IFCALC, IFOBS
      DOUBLE PRECISION WSUM, DSUM, CSUM, DERIV, CORR
      CHARACTER*30 LINE
      INTEGER LLINE
      DOUBLE COMPLEX DBCOMP
C parameters
      DOUBLE PRECISION ZERO, ONE, TWO, THREE, FOUR
      PARAMETER (ZERO=0.0D0, ONE=1.0D0, TWO=2.0D0, THREE=3.0D0)
      PARAMETER (FOUR=4.0D0)
C begin
C
C initialize correlation coefficients
      WSUM=ZERO
      CI=ZERO
      CJ=ZERO
      CII=ZERO
      CJJ=ZERO
      CIJ=ZERO
      IF (XRTRGT.EQ.'F2F2') THEN
      DO 17790 REFLCT=1,XRIREF
      IF (TEST(REFLCT).EQ.ITEST) THEN
C
C compute F^2's
      IFOBS=DREAL(FOBS(REFLCT))**2+DIMAG(FOBS(REFLCT))**2
      IFCALC=DREAL(FCALC(REFLCT)+FPART(REFLCT))**2
     @      +DIMAG(FCALC(REFLCT)+FPART(REFLCT))**2
C
C accumulate information for weighted correlation coefficients
      WSUM=WSUM+WEIGHT(REFLCT)
      CI=CI+WEIGHT(REFLCT)*IFOBS
      CJ=CJ+WEIGHT(REFLCT)*IFCALC
      CII=CII+WEIGHT(REFLCT)*IFOBS**2
      CJJ=CJJ+WEIGHT(REFLCT)*IFCALC**2
      CIJ=CIJ+WEIGHT(REFLCT)*IFOBS*IFCALC
      END IF
17790 CONTINUE
      ELSE
      DO 17791 REFLCT=1,XRIREF
      IF (TEST(REFLCT).EQ.ITEST) THEN
C
C compute |F|'s
      IFOBS=SQRT(DREAL(FOBS(REFLCT))**2+DIMAG(FOBS(REFLCT))**2)
      IFCALC=SQRT(DREAL(FCALC(REFLCT)+FPART(REFLCT))**2
     @      +DIMAG(FCALC(REFLCT)+FPART(REFLCT))**2)
C
C accumulate information for weighted correlation coefficients
      WSUM=WSUM+WEIGHT(REFLCT)
      CI=CI+WEIGHT(REFLCT)*IFOBS
      CJ=CJ+WEIGHT(REFLCT)*IFCALC
      CII=CII+WEIGHT(REFLCT)*IFOBS**2
      CJJ=CJJ+WEIGHT(REFLCT)*IFCALC**2
      CIJ=CIJ+WEIGHT(REFLCT)*IFOBS*IFCALC
      END IF
17791 CONTINUE
      END IF
C
C do some checking
      IF (ABS(CI).LT.RSMALL) THEN
      WRITE(6,'(A,I3,A)')
     @ ' %TRF2F2-error: sum over WEIGHT*FOBS is zero (for TEST=',
     @ ITEST,')'
      ELSE IF (ABS(CJ).LT.RSMALL) THEN
      WRITE(6,'(A,I3,A)')
     @' %TRF2F2-error: sum over WEIGHT*(FCALC+FPART) is 0 (for TEST=',
     @ ITEST,')'
      ELSE
C
C compute weighted correlation coefficient
      DSUM=(CII-CI**2/WSUM)*(CJJ-CJ**2/WSUM)
      CSUM=CIJ - CI*CJ/WSUM
      IF (DSUM.GT.RSMALL) THEN
      DSUM=SQRT(DSUM)
      CORR=CSUM/DSUM
      ELSE
      CORR=ZERO
      END IF
C
C store in energy term
      XRE=XRSCAL*(ONE-CORR)
C
C compute derivatives if required
      IF (QDERIV) THEN
C
C compute derivatives for F's
      IF (XRTRGT.EQ.'F2F2') THEN
      DO 17792 REFLCT=1,XRIREF
      IF (TEST(REFLCT).EQ.ITEST) THEN
C
C compute amplitudes
      IFOBS=DREAL(FOBS(REFLCT))**2+DIMAG(FOBS(REFLCT))**2
      IFCALC=DREAL(FCALC(REFLCT)+FPART(REFLCT))**2
     @      +DIMAG(FCALC(REFLCT)+FPART(REFLCT))**2
C
C compute derivative with respect to FCALC(H)
      IF (DSUM.GT.RSMALL) THEN
      DERIV=-TWO*XRSCAL*WEIGHT(REFLCT)*( (IFOBS-CI/WSUM)/DSUM -
     @      (CORR/DSUM**2)*(CII-CI**2/WSUM)*(IFCALC-CJ/WSUM) )
      ELSE
      DERIV=ZERO
      END IF
      FCALC(REFLCT)=(FCALC(REFLCT)+FPART(REFLCT))*DERIV
      ELSE
      FCALC(REFLCT)=ZERO
      END IF
17792 CONTINUE
      ELSE
      DO 17793 REFLCT=1,XRIREF
      IF (TEST(REFLCT).EQ.ITEST) THEN
C
C compute amplitudes
      IFOBS=SQRT(DREAL(FOBS(REFLCT))**2+DIMAG(FOBS(REFLCT))**2)
      IFCALC=SQRT(DREAL(FCALC(REFLCT)+FPART(REFLCT))**2
     @           +DIMAG(FCALC(REFLCT)+FPART(REFLCT))**2)
C
C compute derivative with respect to |FCALC|(H)
      IF (DSUM.GT.RSMALL.AND.IFCALC.GT.RSMALL) THEN
      DERIV=-XRSCAL*WEIGHT(REFLCT)*( (IFOBS-CI/WSUM)/DSUM -
     @      (CORR/DSUM**2)*(CII-CI**2/WSUM)*(IFCALC-CJ/WSUM) ) /
     @       IFCALC
      ELSE
      DERIV=ZERO
      END IF
      FCALC(REFLCT)=(FCALC(REFLCT)+FPART(REFLCT))*DERIV
      ELSE
      FCALC(REFLCT)=ZERO
      END IF
17793 CONTINUE
      END IF
      END IF
C
      IF (QPRINT) THEN
      IF (XCVTEST.AND.ITEST.EQ.0) THEN
      CALL DECLAR( 'CORR', 'DP', ' ', DBCOMP, CORR )
      LINE=' ->[WORKING SET (TEST=0)]'
      LLINE=25
      ELSEIF (XCVTEST.AND.ITEST.EQ.1) THEN
      CALL DECLAR( 'TEST_CORR', 'DP', ' ', DBCOMP, CORR )
      LINE=' ->[TEST SET (TEST=1)]   '
      LLINE=22
      ELSE
      CALL DECLAR( 'CORR', 'DP', ' ', DBCOMP, CORR )
      LINE=' '
      LLINE=1
      END IF
      IF (XRTRGT.EQ.'F2F2') THEN
      WRITE(6,'(3A,F12.3)')
     @ ' TRF2F2:',LINE(1:LLINE),
     @ ' Corr<F(obs)^2, F(calc)^2> =',CORR
      ELSE
      WRITE(6,'(3A,F12.3)')
     @ ' TRF2F2:',LINE(1:LLINE),
     @ ' Corr<|F(obs)|, |F(calc)|> =',CORR
      END IF
      END IF
C
      END IF
      RETURN
      END
