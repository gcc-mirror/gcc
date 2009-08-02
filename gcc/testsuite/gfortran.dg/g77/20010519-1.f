c { dg-do compile }
CHARMM Element source/dimb/nmdimb.src 1.1
C.##IF DIMB
      SUBROUTINE NMDIMB(X,Y,Z,NAT3,BNBND,BIMAG,LNOMA,AMASS,DDS,DDSCR,
     1                 PARDDV,DDV,DDM,PARDDF,DDF,PARDDE,DDEV,DD1BLK,
     2                 DD1BLL,NADD,LRAISE,DD1CMP,INBCMP,JNBCMP,
     3                 NPAR,ATMPAR,ATMPAS,BLATOM,PARDIM,NFREG,NFRET,
     4                 PARFRQ,CUTF1,ITMX,TOLDIM,IUNMOD,IUNRMD,
     5                 LBIG,LSCI,ATMPAD,SAVF,NBOND,IB,JB,DDVALM)
C-----------------------------------------------------------------------
C     01-Jul-1992 David Perahia, Liliane Mouawad
C     15-Dec-1994 Herman van Vlijmen
C
C     This is the main routine for the mixed-basis diagonalization.
C     See: L.Mouawad and D.Perahia, Biopolymers (1993), 33, 599,
C     and: D.Perahia and L.Mouawad, Comput. Chem. (1995), 19, 241.
C     The method iteratively solves the diagonalization of the
C     Hessian matrix. To save memory space, it uses a compressed
C     form of the Hessian, which only contains the nonzero elements.
C     In the diagonalization process, approximate eigenvectors are
C     mixed with Cartesian coordinates to form a reduced basis. The
C     Hessian is then diagonalized in the reduced basis. By iterating
C     over different sets of Cartesian coordinates the method ultimately
C     converges to the exact eigenvalues and eigenvectors (up to the
C     requested accuracy).
C     If no existing basis set is read, an initial basis will be created
C     which consists of the low-frequency eigenvectors of diagonal blocks
C     of the Hessian.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/impnon.fcm'
C..##IF VAX IRIS HPUX IRIS GNU CSPP OS2 GWS CRAY ALPHA
      IMPLICIT NONE
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/stream.fcm'
      LOGICAL LOWER,QLONGL
      INTEGER MXSTRM,POUTU
      PARAMETER (MXSTRM=20,POUTU=6)
      INTEGER   NSTRM,ISTRM,JSTRM,OUTU,PRNLEV,WRNLEV,IOLEV
      COMMON /CASE/   LOWER, QLONGL
      COMMON /STREAM/ NSTRM,ISTRM,JSTRM(MXSTRM),OUTU,PRNLEV,WRNLEV,IOLEV
C..##IF SAVEFCM
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/dimens.fcm'
      INTEGER LARGE,MEDIUM,SMALL,REDUCE
C..##IF QUANTA
C..##ELIF T3D
C..##ELSE
      PARAMETER (LARGE=60120, MEDIUM=25140, SMALL=6120)
C..##ENDIF
      PARAMETER (REDUCE=15000)
      INTEGER SIZE
C..##IF XLARGE
C..##ELIF XXLARGE
C..##ELIF LARGE
C..##ELIF MEDIUM
      PARAMETER (SIZE=MEDIUM)
C..##ELIF REDUCE
C..##ELIF SMALL
C..##ELIF XSMALL
C..##ENDIF
C..##IF MMFF
      integer MAXDEFI
      parameter(MAXDEFI=250)
      INTEGER NAME0,NAMEQ0,NRES0,KRES0
      PARAMETER (NAME0=4,NAMEQ0=10,NRES0=4,KRES0=4)
      integer MaxAtN
      parameter (MaxAtN=55)
      INTEGER MAXAUX
      PARAMETER (MAXAUX = 10)
C..##ENDIF
      INTEGER MAXCSP, MAXHSET
C..##IF HMCM
      PARAMETER (MAXHSET = 200)
C..##ELSE
C..##ENDIF
C..##IF REDUCE
C..##ELSE
      PARAMETER (MAXCSP = 500)
C..##ENDIF
C..##IF HMCM
      INTEGER MAXHCM,MAXPCM,MAXRCM
C...##IF REDUCE
C...##ELSE
      PARAMETER (MAXHCM=500)
      PARAMETER (MAXPCM=5000)
      PARAMETER (MAXRCM=2000)
C...##ENDIF
C..##ENDIF
      INTEGER MXCMSZ
C..##IF IBM IBMRS CRAY INTEL IBMSP T3D REDUCE
C..##ELSE
      PARAMETER (MXCMSZ = 5000)
C..##ENDIF
      INTEGER CHRSIZ
      PARAMETER (CHRSIZ = SIZE)
      INTEGER MAXATB
C..##IF REDUCE
C..##ELIF QUANTA
C..##ELSE
      PARAMETER (MAXATB = 200)
C..##ENDIF
      INTEGER MAXVEC
C..##IFN VECTOR PARVECT
      PARAMETER (MAXVEC = 10)
C..##ELIF LARGE XLARGE XXLARGE
C..##ELIF MEDIUM
C..##ELIF SMALL REDUCE
C..##ELIF XSMALL
C..##ELSE
C..##ENDIF
      INTEGER IATBMX
      PARAMETER (IATBMX = 8)
      INTEGER MAXHB
C..##IF LARGE XLARGE XXLARGE
C..##ELIF MEDIUM
      PARAMETER (MAXHB = 8000)
C..##ELIF SMALL
C..##ELIF REDUCE XSMALL
C..##ELSE
C..##ENDIF
      INTEGER MAXTRN,MAXSYM
C..##IFN NOIMAGES
      PARAMETER (MAXTRN = 5000)
      PARAMETER (MAXSYM = 192)
C..##ELSE
C..##ENDIF
C..##IF LONEPAIR (lonepair_max)
      INTEGER MAXLP,MAXLPH
C...##IF REDUCE
C...##ELSE
      PARAMETER (MAXLP  = 2000)
      PARAMETER (MAXLPH = 4000)
C...##ENDIF
C..##ENDIF (lonepair_max)
      INTEGER NOEMAX,NOEMX2
C..##IF REDUCE
C..##ELSE
      PARAMETER (NOEMAX = 2000)
      PARAMETER (NOEMX2 = 4000)
C..##ENDIF
      INTEGER MAXATC, MAXCB, MAXCH, MAXCI, MAXCP, MAXCT, MAXITC, MAXNBF
C..##IF REDUCE
C..##ELIF MMFF CFF
      PARAMETER (MAXATC = 500, MAXCB = 1500, MAXCH = 3200, MAXCI = 600,
     &           MAXCP  = 3000,MAXCT = 15500,MAXITC = 200, MAXNBF=1000)
C..##ELIF YAMMP
C..##ELIF LARGE
C..##ELSE
C..##ENDIF
      INTEGER MAXCN
      PARAMETER (MAXCN = MAXITC*(MAXITC+1)/2)
      INTEGER MAXA, MAXAIM, MAXB, MAXT, MAXP
      INTEGER MAXIMP, MAXNB, MAXPAD, MAXRES
      INTEGER MAXSEG, MAXGRP
C..##IF LARGE XLARGE XXLARGE
C..##ELIF MEDIUM
      PARAMETER (MAXA = SIZE, MAXB = SIZE, MAXT = SIZE,
     &           MAXP = 2*SIZE)
      PARAMETER (MAXIMP = 9200, MAXNB = 17200, MAXPAD = 8160,
     &           MAXRES = 14000)
C...##IF MCSS
C...##ELSE
      PARAMETER (MAXSEG = 1000)
C...##ENDIF
C..##ELIF SMALL
C..##ELIF XSMALL
C..##ELIF REDUCE
C..##ELSE
C..##ENDIF
C..##IF NOIMAGES
C..##ELSE
      PARAMETER (MAXAIM = 2*SIZE)
      PARAMETER (MAXGRP = 2*SIZE/3)
C..##ENDIF
      INTEGER REDMAX,REDMX2
C..##IF REDUCE
C..##ELSE
      PARAMETER (REDMAX = 20)
      PARAMETER (REDMX2 = 80)
C..##ENDIF
      INTEGER MXRTRS, MXRTA, MXRTB, MXRTT, MXRTP, MXRTI, MXRTX,
     &        MXRTHA, MXRTHD, MXRTBL, NICM
      PARAMETER (MXRTRS = 200, MXRTA = 5000, MXRTB = 5000,
     &           MXRTT = 5000, MXRTP = 5000, MXRTI = 2000,
C..##IF YAMMP
C..##ELSE
     &           MXRTX = 5000, MXRTHA = 300, MXRTHD = 300,
C..##ENDIF
     &           MXRTBL = 5000, NICM = 10)
      INTEGER NMFTAB,  NMCTAB,  NMCATM,  NSPLIN
C..##IF REDUCE
C..##ELSE
      PARAMETER (NMFTAB = 200, NMCTAB = 3, NMCATM = 12000, NSPLIN = 3)
C..##ENDIF
      INTEGER MAXSHK
C..##IF XSMALL
C..##ELIF REDUCE
C..##ELSE
      PARAMETER (MAXSHK = SIZE*3/4)
C..##ENDIF
      INTEGER SCRMAX
C..##IF IBM IBMRS CRAY INTEL IBMSP T3D REDUCE
C..##ELSE
      PARAMETER (SCRMAX = 5000)
C..##ENDIF
C..##IF TSM
      INTEGER MXPIGG
C...##IF REDUCE
C...##ELSE
      PARAMETER (MXPIGG=500)
C...##ENDIF
      INTEGER MXCOLO,MXPUMB
      PARAMETER (MXCOLO=20,MXPUMB=20)
C..##ENDIF
C..##IF ADUMB
      INTEGER MAXUMP, MAXEPA, MAXNUM
C...##IF REDUCE
C...##ELSE
      PARAMETER (MAXUMP = 10, MAXNUM = 4)
C...##ENDIF
C..##ENDIF
      INTEGER MAXING
      PARAMETER (MAXING=1000)
C..##IF MMFF
      integer MAX_RINGSIZE, MAX_EACH_SIZE
      parameter (MAX_RINGSIZE = 20, MAX_EACH_SIZE = 1000)
      integer MAXPATHS
      parameter (MAXPATHS = 8000)
      integer MAX_TO_SEARCH
      parameter (MAX_TO_SEARCH = 6)
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/number.fcm'
      REAL(KIND=8)     ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX,
     &           SEVEN, EIGHT, NINE, TEN, ELEVEN, TWELVE, THIRTN,
     &           FIFTN, NINETN, TWENTY, THIRTY
C..##IF SINGLE
C..##ELSE
      PARAMETER (ZERO   =  0.D0, ONE    =  1.D0, TWO    =  2.D0,
     &           THREE  =  3.D0, FOUR   =  4.D0, FIVE   =  5.D0,
     &           SIX    =  6.D0, SEVEN  =  7.D0, EIGHT  =  8.D0,
     &           NINE   =  9.D0, TEN    = 10.D0, ELEVEN = 11.D0,
     &           TWELVE = 12.D0, THIRTN = 13.D0, FIFTN  = 15.D0,
     &           NINETN = 19.D0, TWENTY = 20.D0, THIRTY = 30.D0)
C..##ENDIF
      REAL(KIND=8)     FIFTY, SIXTY, SVNTY2, EIGHTY, NINETY, HUNDRD,
     &           ONE2TY, ONE8TY, THRHUN, THR6TY, NINE99, FIFHUN, THOSND,
     &           FTHSND,MEGA
C..##IF SINGLE
C..##ELSE
      PARAMETER (FIFTY  = 50.D0,  SIXTY  =  60.D0,  SVNTY2 =   72.D0,
     &           EIGHTY = 80.D0,  NINETY =  90.D0,  HUNDRD =  100.D0,
     &           ONE2TY = 120.D0, ONE8TY = 180.D0,  THRHUN =  300.D0,
     &           THR6TY=360.D0,   NINE99 = 999.D0,  FIFHUN = 1500.D0,
     &           THOSND = 1000.D0,FTHSND = 5000.D0, MEGA   =   1.0D6)
C..##ENDIF
      REAL(KIND=8)     MINONE, MINTWO, MINSIX
      PARAMETER (MINONE = -1.D0,  MINTWO = -2.D0,  MINSIX = -6.D0)
      REAL(KIND=8) TENM20,TENM14,TENM8,TENM5,PT0001,PT0005,PT001,PT005,
     &           PT01, PT02, PT05, PTONE, PT125, PT25, SIXTH, THIRD,
     &           PTFOUR, PTSIX, HALF, PT75, PT9999, ONEPT5, TWOPT4
C..##IF SINGLE
C..##ELSE
      PARAMETER (TENM20 = 1.0D-20,  TENM14 = 1.0D-14,  TENM8  = 1.0D-8,
     &           TENM5  = 1.0D-5,   PT0001 = 1.0D-4, PT0005 = 5.0D-4,
     &           PT001  = 1.0D-3,   PT005  = 5.0D-3, PT01   = 0.01D0,
     &           PT02   = 0.02D0,   PT05   = 0.05D0, PTONE  = 0.1D0,
     &           PT125  = 0.125D0,  SIXTH  = ONE/SIX,PT25   = 0.25D0,
     &           THIRD  = ONE/THREE,PTFOUR = 0.4D0,  HALF   = 0.5D0,
     &           PTSIX  = 0.6D0,    PT75   = 0.75D0, PT9999 = 0.9999D0,
     &           ONEPT5 = 1.5D0,    TWOPT4 = 2.4D0)
C..##ENDIF
      REAL(KIND=8) ANUM,FMARK
      REAL(KIND=8) RSMALL,RBIG
C..##IF SINGLE
C..##ELSE
      PARAMETER (ANUM=9999.0D0, FMARK=-999.0D0)
      PARAMETER (RSMALL=1.0D-10,RBIG=1.0D20)
C..##ENDIF
      REAL(KIND=8) RPRECI,RBIGST
C..##IF VAX DEC
C..##ELIF IBM
C..##ELIF CRAY
C..##ELIF ALPHA T3D T3E
C..##ELSE
C...##IF SINGLE
C...##ELSE
      PARAMETER (RPRECI = 2.22045D-16, RBIGST = 4.49423D+307)
C...##ENDIF
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/consta.fcm'
      REAL(KIND=8) PI,RADDEG,DEGRAD,TWOPI
      PARAMETER(PI=3.141592653589793D0,TWOPI=2.0D0*PI)
      PARAMETER (RADDEG=180.0D0/PI)
      PARAMETER (DEGRAD=PI/180.0D0)
      REAL(KIND=8) COSMAX
      PARAMETER (COSMAX=0.9999999999D0)
      REAL(KIND=8) TIMFAC
      PARAMETER (TIMFAC=4.88882129D-02)
      REAL(KIND=8) KBOLTZ
      PARAMETER (KBOLTZ=1.987191D-03)
      REAL(KIND=8) CCELEC
C..##IF AMBER
C..##ELIF DISCOVER
C..##ELSE
      PARAMETER (CCELEC=332.0716D0)
C..##ENDIF
      REAL(KIND=8) CNVFRQ
      PARAMETER (CNVFRQ=2045.5D0/(2.99793D0*6.28319D0))
      REAL(KIND=8) SPEEDL
      PARAMETER (SPEEDL=2.99793D-02)
      REAL(KIND=8) ATMOSP
      PARAMETER (ATMOSP=1.4584007D-05)
      REAL(KIND=8) PATMOS
      PARAMETER (PATMOS = 1.D0 / ATMOSP )
      REAL(KIND=8) BOHRR
      PARAMETER (BOHRR = 0.529177249D0 )
      REAL(KIND=8) TOKCAL
      PARAMETER (TOKCAL = 627.5095D0 )
C..##IF MMFF
      REAL(KIND=8) MDAKCAL
      parameter(MDAKCAL=143.9325D0)
C..##ENDIF
      REAL(KIND=8) DEBYEC
      PARAMETER ( DEBYEC = 2.541766D0 / BOHRR )
      REAL(KIND=8) ZEROC
      PARAMETER ( ZEROC = 298.15D0 )
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/exfunc.fcm'
C..##IF ACE
C..##ENDIF
C..##IF ADUMB
C..##ENDIF
      CHARACTER(4) GTRMA, NEXTA4, CURRA4
      CHARACTER(6) NEXTA6
      CHARACTER(8) NEXTA8
      CHARACTER(20) NEXT20
      INTEGER     ALLCHR, ALLSTK, ALLHP, DECODI, FIND52,
     *            GETATN, GETRES, GETRSN, GETSEG, GTRMI, I4VAL,
     *            ICHAR4, ICMP16, ILOGI4, INDX, INDXA, INDXAF,
     *            INDXRA, INTEG4, IREAL4, IREAL8, LOCDIF,
     *            LUNASS, MATOM, NEXTI, NINDX, NSELCT, NSELCTV, ATMSEL,
     *            PARNUM, PARINS,
     *            SRCHWD, SRCHWS, STRLNG, DSIZE, SSIZE
C..##IF ACE
     *           ,GETNNB
C..##ENDIF
      LOGICAL     CHKPTR, EQST, EQSTA, EQSTWC, EQWDWC, DOTRIM, CHECQUE,
     *            HYDROG, INITIA, LONE, LTSTEQ, ORDER, ORDER5,
     *            ORDERR, USEDDT, QTOKDEL, QDIGIT, QALPHA
      REAL(KIND=8)      DECODF, DOTVEC, GTRMF, LENVEC, NEXTF, RANDOM, GTRR8,
     *            RANUMB, R8VAL, RETVAL8, SUMVEC
C..##IF ADUMB
     *           ,UMFI
C..##ENDIF
      EXTERNAL  GTRMA, NEXTA4, CURRA4, NEXTA6, NEXTA8,NEXT20,
     *          ALLCHR, ALLSTK, ALLHP, DECODI, FIND52,
     *          GETATN, GETRES, GETRSN, GETSEG, GTRMI, I4VAL,
     *          ICHAR4, ICMP16,  ILOGI4, INDX, INDXA, INDXAF,
     *          INDXRA, INTEG4, IREAL4, IREAL8, LOCDIF,
     *          LUNASS, MATOM, NEXTI, NINDX, NSELCT, NSELCTV, ATMSEL,
     *          PARNUM, PARINS,
     *          SRCHWD, SRCHWS, STRLNG, DSIZE, SSIZE,
     *          CHKPTR, EQST, EQSTA, EQSTWC, EQWDWC, DOTRIM, CHECQUE,
     *          HYDROG, INITIA, LONE, LTSTEQ, ORDER, ORDER5,
     *          ORDERR, USEDDT, QTOKDEL, QDIGIT, QALPHA,
     *          DECODF, DOTVEC, GTRMF, LENVEC, NEXTF, RANDOM, GTRR8,
     *          RANUMB, R8VAL, RETVAL8, SUMVEC
C..##IF ADUMB
     *           ,UMFI
C..##ENDIF
C..##IF ACE
     *           ,GETNNB
C..##ENDIF
C..##IFN NOIMAGES
      INTEGER IMATOM
      EXTERNAL IMATOM
C..##ENDIF
C..##IF MBOND
C..##ENDIF
C..##IF MMFF
      INTEGER LEN_TRIM
      EXTERNAL LEN_TRIM
      CHARACTER(4) AtName
      external AtName
      CHARACTER(8) ElementName
      external ElementName
      CHARACTER(10) QNAME
      external QNAME
      integer  IATTCH, IBORDR, CONN12, CONN13, CONN14
      integer  LEQUIV, LPATH
      integer  nbndx, nbnd2, nbnd3, NTERMA
      external IATTCH, IBORDR, CONN12, CONN13, CONN14
      external LEQUIV, LPATH
      external nbndx, nbnd2, nbnd3, NTERMA
      external find_loc
      REAL(KIND=8)   vangle, OOPNGL, TORNGL, ElementMass
      external vangle, OOPNGL, TORNGL, ElementMass
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/stack.fcm'
      INTEGER STKSIZ
C..##IFN UNICOS
C...##IF LARGE XLARGE
C...##ELIF MEDIUM REDUCE
      PARAMETER (STKSIZ=4000000)
C...##ELIF SMALL
C...##ELIF XSMALL
C...##ELIF XXLARGE
C...##ELSE
C...##ENDIF
      INTEGER LSTUSD,MAXUSD,STACK
      COMMON /ISTACK/ LSTUSD,MAXUSD,STACK(STKSIZ)
C..##ELSE
C..##ENDIF
C..##IF SAVEFCM
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/heap.fcm'
      INTEGER HEAPDM
C..##IFN UNICOS (unicos)
C...##IF XXLARGE (size)
C...##ELIF LARGE XLARGE (size)
C...##ELIF MEDIUM (size)
C....##IF T3D (t3d2)
C....##ELIF TERRA (t3d2)
C....##ELIF ALPHA (t3d2)
C....##ELIF T3E (t3d2)
C....##ELSE (t3d2)
      PARAMETER (HEAPDM=2048000)
C....##ENDIF (t3d2)
C...##ELIF SMALL (size)
C...##ELIF REDUCE (size)
C...##ELIF XSMALL (size)
C...##ELSE (size)
C...##ENDIF (size)
      INTEGER FREEHP,HEAPSZ,HEAP
      COMMON /HEAPST/ FREEHP,HEAPSZ,HEAP(HEAPDM)
      LOGICAL LHEAP(HEAPDM)
      EQUIVALENCE (LHEAP,HEAP)
C..##ELSE (unicos)
C..##ENDIF (unicos)
C..##IF SAVEFCM (save)
C..##ENDIF (save)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/fast.fcm'
      INTEGER IACNB, NITCC, ICUSED, FASTER, LFAST, LMACH, OLMACH
      INTEGER ICCOUNT, LOWTP, IGCNB, NITCC2
      INTEGER ICCNBA, ICCNBB, ICCNBC, ICCNBD, LCCNBA, LCCNBD
      COMMON /FASTI/ FASTER, LFAST, LMACH, OLMACH, NITCC, NITCC2,
     &               ICUSED(MAXATC), ICCOUNT(MAXATC), LOWTP(MAXATC),
     &               IACNB(MAXAIM), IGCNB(MAXATC),
     &               ICCNBA, ICCNBB, ICCNBC, ICCNBD, LCCNBA, LCCNBD
C..##IF SAVEFCM
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/deriv.fcm'
      REAL(KIND=8) DX,DY,DZ
      COMMON /DERIVR/ DX(MAXAIM),DY(MAXAIM),DZ(MAXAIM)
C..##IF SAVEFCM
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/energy.fcm'
      INTEGER LENENP, LENENT, LENENV, LENENA
      PARAMETER (LENENP = 50, LENENT = 70, LENENV = 50,
     &           LENENA = LENENP + LENENT + LENENV )
      INTEGER TOTE, TOTKE, EPOT, TEMPS, GRMS, BPRESS, PJNK1, PJNK2,
     &        PJNK3, PJNK4, HFCTE, HFCKE, EHFC, EWORK, VOLUME, PRESSE,
     &        PRESSI, VIRI, VIRE, VIRKE, TEPR, PEPR, KEPR, KEPR2,
     &        DROFFA,
     &        XTLTE, XTLKE, XTLPE, XTLTEM, XTLPEP, XTLKEP, XTLKP2,
     &        TOT4, TOTK4, EPOT4, TEM4, MbMom, BodyT, PartT
C..##IF ACE
     &      , SELF, SCREEN, COUL ,SOLV, INTER
C..##ENDIF
C..##IF FLUCQ
     &       ,FQKIN
C..##ENDIF
      PARAMETER (TOTE   =  1, TOTKE  =  2, EPOT   =  3, TEMPS  =  4,
     &           GRMS   =  5, BPRESS =  6, PJNK1  =  7, PJNK2  =  8,
     &           PJNK3  =  9, PJNK4  = 10, HFCTE  = 11, HFCKE  = 12,
     &           EHFC   = 13, EWORK  = 11, VOLUME = 15, PRESSE = 16,
     &           PRESSI = 17, VIRI   = 18, VIRE   = 19, VIRKE  = 20,
     &           TEPR   = 21, PEPR   = 22, KEPR   = 23, KEPR2  = 24,
     &                        DROFFA = 26, XTLTE  = 27, XTLKE  = 28,
     &           XTLPE  = 29, XTLTEM = 30, XTLPEP = 31, XTLKEP = 32,
     &           XTLKP2 = 33,
     &           TOT4   = 37, TOTK4  = 38, EPOT4  = 39, TEM4   = 40,
     &           MbMom  = 41, BodyT  = 42, PartT  = 43
C..##IF ACE
     &         , SELF   = 45, SCREEN = 46, COUL   = 47,
     &           SOLV   = 48, INTER  = 49
C..##ENDIF
C..##IF FLUCQ
     &          ,FQKIN  = 50
C..##ENDIF
     &          )
C..##IF ACE
C..##ENDIF
C..##IF GRID
C..##ENDIF
C..##IF FLUCQ
C..##ENDIF
      INTEGER  BOND, ANGLE, UREYB, DIHE, IMDIHE, VDW, ELEC, HBOND,
     &         USER, CHARM, CDIHE, CINTCR, CQRT, NOE, SBNDRY,
     &         IMVDW, IMELEC, IMHBND, EWKSUM, EWSELF, EXTNDE, RXNFLD,
     &         ST2, IMST2, TSM, QMEL, QMVDW, ASP, EHARM, GEO, MDIP,
     &         PRMS, PANG, SSBP, BK4D, SHEL, RESD, SHAP,
     &         STRB, OOPL, PULL, POLAR, DMC, RGY, EWEXCL, EWQCOR,
     &         EWUTIL, PBELEC, PBNP, PINT, MbDefrm, MbElec, STRSTR,
     &         BNDBND, BNDTW, EBST, MBST, BBT, SST, GBEnr, GSBP
C..##IF HMCM
     &       , HMCM
C..##ENDIF
C..##IF ADUMB
     &       , ADUMB
C..##ENDIF
     &       , HYDR
C..##IF FLUCQ
     &       , FQPOL
C..##ENDIF
      PARAMETER (BOND   =  1, ANGLE  =  2, UREYB  =  3, DIHE   =  4,
     &           IMDIHE =  5, VDW    =  6, ELEC   =  7, HBOND  =  8,
     &           USER   =  9, CHARM  = 10, CDIHE  = 11, CINTCR = 12,
     &           CQRT   = 13, NOE    = 14, SBNDRY = 15, IMVDW  = 16,
     &           IMELEC = 17, IMHBND = 18, EWKSUM = 19, EWSELF = 20,
     &           EXTNDE = 21, RXNFLD = 22, ST2    = 23, IMST2  = 24,
     &           TSM    = 25, QMEL   = 26, QMVDW  = 27, ASP    = 28,
     &           EHARM  = 29, GEO    = 30, MDIP   = 31, PINT   = 32,
     &           PRMS   = 33, PANG   = 34, SSBP   = 35, BK4D   = 36,
     &           SHEL   = 37, RESD   = 38, SHAP   = 39, STRB   = 40,
     &           OOPL   = 41, PULL   = 42, POLAR  = 43, DMC    = 44,
     &           RGY    = 45, EWEXCL = 46, EWQCOR = 47, EWUTIL = 48,
     &           PBELEC = 49, PBNP   = 50, MbDefrm= 51, MbElec = 52,
     &           STRSTR = 53, BNDBND = 54, BNDTW  = 55, EBST   = 56,
     &           MBST   = 57, BBT    = 58, SST    = 59, GBEnr  = 60,
     &           GSBP   = 65
C..##IF HMCM
     &         , HMCM   = 61
C..##ENDIF
C..##IF ADUMB
     &         , ADUMB  = 62
C..##ENDIF
     &         , HYDR   = 63
C..##IF FLUCQ
     &         , FQPOL  = 65
C..##ENDIF
     &           )
      INTEGER  VEXX, VEXY, VEXZ, VEYX, VEYY, VEYZ, VEZX, VEZY, VEZZ,
     &         VIXX, VIXY, VIXZ, VIYX, VIYY, VIYZ, VIZX, VIZY, VIZZ,
     &         PEXX, PEXY, PEXZ, PEYX, PEYY, PEYZ, PEZX, PEZY, PEZZ,
     &         PIXX, PIXY, PIXZ, PIYX, PIYY, PIYZ, PIZX, PIZY, PIZZ
      PARAMETER ( VEXX =  1, VEXY =  2, VEXZ =  3, VEYX =  4,
     &            VEYY =  5, VEYZ =  6, VEZX =  7, VEZY =  8,
     &            VEZZ =  9,
     &            VIXX = 10, VIXY = 11, VIXZ = 12, VIYX = 13,
     &            VIYY = 14, VIYZ = 15, VIZX = 16, VIZY = 17,
     &            VIZZ = 18,
     &            PEXX = 19, PEXY = 20, PEXZ = 21, PEYX = 22,
     &            PEYY = 23, PEYZ = 24, PEZX = 25, PEZY = 26,
     &            PEZZ = 27,
     &            PIXX = 28, PIXY = 29, PIXZ = 30, PIYX = 31,
     &            PIYY = 32, PIYZ = 33, PIZX = 34, PIZY = 35,
     &            PIZZ = 36)
      CHARACTER(4)  CEPROP, CETERM, CEPRSS
      COMMON /ANER/ CEPROP(LENENP), CETERM(LENENT), CEPRSS(LENENV)
      LOGICAL  QEPROP, QETERM, QEPRSS
      COMMON /QENER/ QEPROP(LENENP), QETERM(LENENT), QEPRSS(LENENV)
      REAL(KIND=8)   EPROP, ETERM, EPRESS
      COMMON /ENER/ EPROP(LENENP), ETERM(LENENT), EPRESS(LENENV)
C..##IF SAVEFCM
C..##ENDIF
      REAL(KIND=8)   EPRPA, EPRP2A, EPRPP, EPRP2P,
     &         ETRMA, ETRM2A, ETRMP, ETRM2P,
     &         EPRSA, EPRS2A, EPRSP, EPRS2P
      COMMON /ENACCM/ EPRPA(LENENP), ETRMA(LENENT), EPRSA(LENENV),
     &                EPRP2A(LENENP),ETRM2A(LENENT),EPRS2A(LENENV),
     &                EPRPP(LENENP), ETRMP(LENENT), EPRSP(LENENV),
     &                EPRP2P(LENENP),ETRM2P(LENENT),EPRS2P(LENENV)
C..##IF SAVEFCM
C..##ENDIF
      INTEGER  ECALLS, TOT1ST, TOT2ND
      COMMON /EMISCI/ ECALLS, TOT1ST, TOT2ND
      REAL(KIND=8)   EOLD, FITA, DRIFTA, EAT0A, CORRA, FITP, DRIFTP,
     &         EAT0P, CORRP
      COMMON /EMISCR/ EOLD, FITA, DRIFTA, EAT0A, CORRA,
     &                     FITP, DRIFTP, EAT0P, CORRP
C..##IF SAVEFCM
C..##ENDIF
C..##IF ACE
C..##ENDIF
C..##IF FLUCQ
C..##ENDIF
C..##IF ADUMB
C..##ENDIF
C..##IF GRID
C..##ENDIF
C..##IF FLUCQ
C..##ENDIF
C..##IF TSM
      REAL(KIND=8) TSMTRM(LENENT),TSMTMP(LENENT)
      COMMON /TSMENG/ TSMTRM,TSMTMP
C...##IF SAVEFCM
C...##ENDIF
C..##ENDIF
      REAL(KIND=8) EHQBM
      LOGICAL HQBM
      COMMON /HQBMVAR/HQBM
C..##IF SAVEFCM
C..##ENDIF
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/dimb.fcm'
C..##IF DIMB (dimbfcm)
      INTEGER NPARMX,MNBCMP,LENDSK
      PARAMETER (NPARMX=1000,MNBCMP=300,LENDSK=200000)
      INTEGER IJXXCM,IJXYCM,IJXZCM,IJYXCM,IJYYCM
      INTEGER IJYZCM,IJZXCM,IJZYCM,IJZZCM
      INTEGER IIXXCM,IIXYCM,IIXZCM,IIYYCM
      INTEGER IIYZCM,IIZZCM
      INTEGER JJXXCM,JJXYCM,JJXZCM,JJYYCM
      INTEGER JJYZCM,JJZZCM
      PARAMETER (IJXXCM=1,IJXYCM=2,IJXZCM=3,IJYXCM=4,IJYYCM=5)
      PARAMETER (IJYZCM=6,IJZXCM=7,IJZYCM=8,IJZZCM=9)
      PARAMETER (IIXXCM=1,IIXYCM=2,IIXZCM=3,IIYYCM=4)
      PARAMETER (IIYZCM=5,IIZZCM=6)
      PARAMETER (JJXXCM=1,JJXYCM=2,JJXZCM=3,JJYYCM=4)
      PARAMETER (JJYZCM=5,JJZZCM=6)
      INTEGER ITER,IPAR1,IPAR2,NFSAV,PINBCM,PJNBCM,PDD1CM,LENCMP
      LOGICAL QDISK,QDW,QCMPCT
      COMMON /DIMBI/ ITER,IPAR1,IPAR2,NFSAV,PINBCM,PJNBCM,LENCMP
      COMMON /DIMBL/ QDISK,QDW,QCMPCT
C...##IF SAVEFCM
C...##ENDIF
C..##ENDIF (dimbfcm)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C:::##INCLUDE '~/charmm_fcm/ctitla.fcm'
      INTEGER MAXTIT
      PARAMETER (MAXTIT=32)
      INTEGER NTITLA,NTITLB
      CHARACTER(80) TITLEA,TITLEB
      COMMON /NTITLA/ NTITLA,NTITLB
      COMMON /CTITLA/ TITLEA(MAXTIT),TITLEB(MAXTIT)
C..##IF SAVEFCM
C..##ENDIF
C-----------------------------------------------------------------------
C Passed variables
      INTEGER NAT3,NADD,NPAR,NFREG,NFRET,BLATOM
      INTEGER ATMPAR(2,*),ATMPAS(2,*),ATMPAD(2,*)
      INTEGER BNBND(*),BIMAG(*)
      INTEGER INBCMP(*),JNBCMP(*),PARDIM
      INTEGER ITMX,IUNMOD,IUNRMD,SAVF
      INTEGER NBOND,IB(*),JB(*)
      REAL(KIND=8) X(*),Y(*),Z(*),AMASS(*),DDSCR(*)
      REAL(KIND=8) DDV(NAT3,*),PARDDV(PARDIM,*),DDM(*),DDS(*)
      REAL(KIND=8) DDF(*),PARDDF(*),DDEV(*),PARDDE(*)
      REAL(KIND=8) DD1BLK(*),DD1BLL(*),DD1CMP(*)
      REAL(KIND=8) TOLDIM,DDVALM
      REAL(KIND=8) PARFRQ,CUTF1
      LOGICAL LNOMA,LRAISE,LSCI,LBIG
C Local variables
      INTEGER NATOM,NATP,NDIM,I,J,II,OLDFAS,OLDPRN,IUPD
      INTEGER NPARC,NPARD,NPARS,NFCUT1,NFREG2,NFREG6
      INTEGER IH1,IH2,IH3,IH4,IH5,IH6,IH7,IH8
      INTEGER IS1,IS2,IS3,IS4,JSPACE,JSP,DDSS,DD5
      INTEGER ISTRT,ISTOP,IPA1,IPA2,IRESF
      INTEGER ATMPAF,INIDS,TRAROT
      INTEGER SUBLIS,ATMCOR
      INTEGER NFRRES,DDVBAS
      INTEGER DDV2,DDVAL
      INTEGER LENCM,NTR,NFRE,NFC,N1,N2,NFCUT,NSUBP
      INTEGER SCIFV1,SCIFV2,SCIFV3,SCIFV4,SCIFV6
      INTEGER DRATQ,ERATQ,E2RATQ,BDRATQ,INRATQ
      INTEGER I620,I640,I660,I700,I720,I760,I800,I840,I880,I920
      REAL(KIND=8) CVGMX,TOLER
      LOGICAL LCARD,LAPPE,LPURG,LWDINI,QCALC,QMASWT,QMIX,QDIAG
C Begin
      QCALC=.TRUE.
      LWDINI=.FALSE.
      INIDS=0
      IS3=0
      IS4=0
      LPURG=.TRUE.
      ITER=0
      NADD=0
      NFSAV=0
      TOLER=TENM5
      QDIAG=.TRUE.
      CVGMX=HUNDRD
      QMIX=.FALSE.
      NATOM=NAT3/3
      NFREG6=(NFREG-6)/NPAR
      NFREG2=NFREG/2
      NFRRES=(NFREG+6)/2
      IF(NFREG.GT.PARDIM) CALL WRNDIE(-3,'<NMDIMB>',
     1     'NFREG IS LARGER THAN PARDIM*3')
C
C ALLOCATE-SPACE-FOR-TRANSROT-VECTORS
      ASSIGN 801 TO I800 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
      GOTO 800
 801  CONTINUE
C ALLOCATE-SPACE-FOR-DIAGONALIZATION
      ASSIGN 721 TO I720 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
      GOTO 720
 721  CONTINUE
C ALLOCATE-SPACE-FOR-REDUCED-BASIS
      ASSIGN 761 TO I760 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
      GOTO 760
 761  CONTINUE
C ALLOCATE-SPACE-FOR-OTHER-ARRAYS
      ASSIGN 921 TO I920 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
      GOTO 920
 921  CONTINUE
C
C Space allocation for working arrays of EISPACK
C diagonalization subroutines
      IF(LSCI) THEN
C ALLOCATE-SPACE-FOR-LSCI
         ASSIGN 841 TO I840 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
         GOTO 840
 841     CONTINUE
      ELSE
C ALLOCATE-DUMMY-SPACE-FOR-LSCI
         ASSIGN 881 TO I880 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
         GOTO 880
 881     CONTINUE
      ENDIF
      QMASWT=(.NOT.LNOMA)
      IF(.NOT. QDISK) THEN
         LENCM=INBCMP(NATOM-1)*9+NATOM*6
         DO I=1,LENCM
            DD1CMP(I)=0.0
         ENDDO
         OLDFAS=LFAST
         QCMPCT=.TRUE.
         LFAST = -1
         CALL ENERGY(X,Y,Z,DX,DY,DZ,BNBND,BIMAG,NAT3,DD1CMP,.TRUE.,1)
         LFAST=OLDFAS
         QCMPCT=.FALSE.
C
C Mass weight DD1CMP matrix
C
         CALL MASSDD(DD1CMP,DDM,INBCMP,JNBCMP,NATOM)
      ELSE
         CALL WRNDIE(-3,'<NMDIMB>','QDISK OPTION NOT SUPPORTED YET')
C         DO I=1,LENDSK
C            DD1CMP(I)=0.0
C         ENDDO
C         OLDFAS=LFAST
C         LFAST = -1
      ENDIF
C
C Fill DDV with six translation-rotation vectors
C
      CALL TRROT(X,Y,Z,DDV,NAT3,1,DDM)
      CALL CPARAY(HEAP(TRAROT),DDV,NAT3,1,6,1)
      NTR=6
      OLDPRN=PRNLEV
      PRNLEV=1
      CALL ORTHNM(1,6,NTR,HEAP(TRAROT),NAT3,.FALSE.,TOLER)
      PRNLEV=OLDPRN
      IF(IUNRMD .LT. 0) THEN
C
C If no previous basis is read
C
         IF(PRNLEV.GE.2) WRITE(OUTU,502) NPAR
 502     FORMAT(/' NMDIMB: Calculating initial basis from block ',
     1           'diagonals'/' NMDIMB: The number of blocks is ',I5/)
         NFRET = 6
         DO I=1,NPAR
            IS1=ATMPAR(1,I)
            IS2=ATMPAR(2,I)
            NDIM=(IS2-IS1+1)*3
            NFRE=NDIM
            IF(NFRE.GT.NFREG6) NFRE=NFREG6
            IF(NFREG6.EQ.0) NFRE=1
            CALL FILUPT(HEAP(IUPD),NDIM)
            CALL MAKDDU(DD1BLK,DD1CMP,INBCMP,JNBCMP,HEAP(IUPD),
     1                  IS1,IS2,NATOM)
            IF(PRNLEV.GE.9) CALL PRINTE(OUTU,EPROP,ETERM,'VIBR',
     1          'ENR',.TRUE.,1,ZERO,ZERO)
C
C Generate the lower section of the matrix and diagonalize
C
C..##IF EISPACK
C..##ENDIF
               IH1=1
               NATP=NDIM+1
               IH2=IH1+NATP
               IH3=IH2+NATP
               IH4=IH3+NATP
               IH5=IH4+NATP
               IH6=IH5+NATP
               IH7=IH6+NATP
               IH8=IH7+NATP
               CALL DIAGQ(NDIM,NFRE,DD1BLK,PARDDV,DDS(IH2),DDS(IH3),
     1           DDS(IH4),DDS(IH5),DDS,DDS(IH6),DDS(IH7),DDS(IH8),NADD)
C..##IF EISPACK
C..##ENDIF
C
C Put the PARDDV vectors into DDV and replace the elements which do
C not belong to the considered partitioned region by zeros.
C
            CALL ADJNME(DDV,PARDDV,NAT3,NDIM,NFRE,NFRET,IS1,IS2)
            IF(LSCI) THEN
               DO J=1,NFRE
               PARDDF(J)=CNVFRQ*SQRT(ABS(PARDDE(J)))
               IF(PARDDE(J) .LT. 0.0) PARDDF(J)=-PARDDF(J)
               ENDDO
            ELSE
               DO J=1,NFRE
               PARDDE(J)=DDS(J)
               PARDDF(J)=CNVFRQ*SQRT(ABS(PARDDE(J)))
               IF(PARDDE(J) .LT. 0.0) PARDDF(J)=-PARDDF(J)
               ENDDO
            ENDIF
            IF(PRNLEV.GE.2) THEN
               WRITE(OUTU,512) I
               WRITE(OUTU,514)
               WRITE(OUTU,516) (J,PARDDF(J),J=1,NFRE)
            ENDIF
            NFRET=NFRET+NFRE
            IF(NFRET .GE. NFREG) GOTO 10
         ENDDO
 512     FORMAT(/' NMDIMB: Diagonalization of part',I5,' completed')
 514     FORMAT(' NMDIMB: Frequencies'/)
 516     FORMAT(5(I4,F12.6))
   10    CONTINUE
C
C Orthonormalize the eigenvectors
C
         OLDPRN=PRNLEV
         PRNLEV=1
         CALL ORTHNM(1,NFRET,NFRET,DDV,NAT3,LPURG,TOLER)
         PRNLEV=OLDPRN
C
C Do reduced basis diagonalization using the DDV vectors
C and get eigenvectors of zero iteration
C
         IF(PRNLEV.GE.2) THEN
            WRITE(OUTU,521) ITER
            WRITE(OUTU,523) NFRET
         ENDIF
 521     FORMAT(/' NMDIMB: Iteration number = ',I5)
 523     FORMAT(' NMDIMB: Dimension of the reduced basis set = ',I5)
         IF(LBIG) THEN
            IF(PRNLEV.GE.2) WRITE(OUTU,585) NFRET,IUNMOD
 525        FORMAT(' NMDIMB: ',I5,' basis vectors are saved in unit',I5)
            REWIND (UNIT=IUNMOD)
            LCARD=.FALSE.
            CALL WRTNMD(LCARD,1,NFRET,NAT3,DDV,DDSCR,DDEV,IUNMOD,AMASS)
            CALL SAVEIT(IUNMOD)
         ELSE
            CALL CPARAY(HEAP(DDVBAS),DDV,NAT3,1,NFRET,1)
         ENDIF
         CALL RBDG(X,Y,Z,NAT3,NDIM,NFRET,DDV,DDF,DDEV,
     1     DDSCR,HEAP(DD5),HEAP(DDSS),HEAP(DDV2),NADD,
     2     INBCMP,JNBCMP,HEAP(DDVBAS),DD1CMP,QMIX,0,0,IS3,IS4,
     3     CUTF1,NFCUT1,NFREG,HEAP(IUPD),DD1BLL,HEAP(SCIFV1),
     4     HEAP(SCIFV2),HEAP(SCIFV3),HEAP(SCIFV4),HEAP(SCIFV6),
     5     HEAP(DRATQ),HEAP(ERATQ),HEAP(E2RATQ),
     6     HEAP(BDRATQ),HEAP(INRATQ),LSCI,LBIG,IUNMOD)
C
C DO-THE-DIAGONALISATIONS-WITH-RESIDUALS
C
         ASSIGN 621 TO I620 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
         GOTO 620
 621     CONTINUE
C SAVE-MODES
         ASSIGN 701 TO I700 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
         GOTO 700
 701     CONTINUE
         IF(ITER.EQ.ITMX) THEN
            CALL CLEANHP(NAT3,NFREG,NPARD,NSUBP,PARDIM,DDV2,DDSS,DDVBAS,
     1                   DDVAL,JSPACE,TRAROT,
     2                   SCIFV1,SCIFV2,SCIFV3,SCIFV4,SCIFV6,
     3                   DRATQ,ERATQ,E2RATQ,BDRATQ,INRATQ,IUPD,ATMPAF,
     4                   ATMCOR,SUBLIS,LSCI,QDW,LBIG)
            RETURN
         ENDIF
      ELSE
C
C Read in existing basis
C
         IF(PRNLEV.GE.2) THEN
            WRITE(OUTU,531)
 531        FORMAT(/' NMDIMB: Calculations restarted')
         ENDIF
C READ-MODES
         ISTRT=1
         ISTOP=99999999
         LCARD=.FALSE.
         LAPPE=.FALSE.
         CALL RDNMD(LCARD,NFRET,NFREG,NAT3,NDIM,
     1     DDV,DDSCR,DDF,DDEV,
     2     IUNRMD,LAPPE,ISTRT,ISTOP)
         NFRET=NDIM
         IF(NFRET.GT.NFREG) THEN
            NFRET=NFREG
            CALL WRNDIE(-1,'<NMDIMB>',
     1       'Not enough space to hold the basis. Increase NMODes')
         ENDIF
C PRINT-MODES
         IF(PRNLEV.GE.2) THEN
            WRITE(OUTU,533) NFRET,IUNRMD
            WRITE(OUTU,514)
            WRITE(OUTU,516) (J,DDF(J),J=1,NFRET)
         ENDIF
 533     FORMAT(/' NMDIMB: ',I5,' restart modes read from unit ',I5)
         NFRRES=NFRET
      ENDIF
C
C -------------------------------------------------
C Here starts the mixed-basis diagonalization part.
C -------------------------------------------------
C
C
C Check cut-off frequency
C
      CALL SELNMD(DDF,NFRET,CUTF1,NFCUT1)
C TEST-NFCUT1
      IF(IUNRMD.LT.0) THEN
        IF(NFCUT1*2-6.GT.NFREG) THEN
           IF(PRNLEV.GE.2) WRITE(OUTU,537) DDF(NFRRES)
           NFCUT1=NFRRES
           CUTF1=DDF(NFRRES)
        ENDIF
      ELSE
        CUTF1=DDF(NFRRES)
      ENDIF
 537  FORMAT(/' NMDIMB: Too many vectors for the given cutoff frequency'
     1       /'         Cutoff frequency is decreased to',F9.3)
C
C Compute the new partioning of the molecule
C
      CALL PARTIC(NAT3,NFREG,NFCUT1,NPARMX,NPARC,ATMPAR,NFRRES,
     1            PARDIM)
      NPARS=NPARC
      DO I=1,NPARC
         ATMPAS(1,I)=ATMPAR(1,I)
         ATMPAS(2,I)=ATMPAR(2,I)
      ENDDO
      IF(QDW) THEN
         IF(IPAR1.EQ.0.OR.IPAR2.EQ.0) LWDINI=.TRUE.
         IF(IPAR1.GE.IPAR2) LWDINI=.TRUE.
         IF(IABS(IPAR1).GT.NPARC*2) LWDINI=.TRUE.
         IF(IABS(IPAR2).GT.NPARC*2) LWDINI=.TRUE.
         IF(ITER.EQ.0) LWDINI=.TRUE.
      ENDIF
      ITMX=ITMX+ITER
      IF(PRNLEV.GE.2) THEN
         WRITE(OUTU,543) ITER,ITMX
         IF(QDW) WRITE(OUTU,545) IPAR1,IPAR2
      ENDIF
 543  FORMAT(/' NMDIMB: Previous iteration number = ',I8/
     1        ' NMDIMB: Iteration number to reach = ',I8)
 545  FORMAT(' NMDIMB: Previous sub-blocks = ',I5,2X,I5)
C
      IF(SAVF.LE.0) SAVF=NPARC
      IF(PRNLEV.GE.2) WRITE(OUTU,547) SAVF
 547  FORMAT(' NMDIMB: Eigenvectors will be saved every',I5,
     1       ' iterations')
C
C If double windowing is defined, the original block sizes are divided
C in two.
C
      IF(QDW) THEN
         NSUBP=1
         CALL PARTID(NPARC,ATMPAR,NPARD,ATMPAD,NPARMX)
         ATMPAF=ALLHP(INTEG4(NPARD*NPARD))
         ATMCOR=ALLHP(INTEG4(NATOM))
         DDVAL=ALLHP(IREAL8(NPARD*NPARD))
         CALL CORARR(ATMPAD,NPARD,HEAP(ATMCOR),NATOM)
         CALL PARLIS(HEAP(ATMCOR),HEAP(ATMPAF),INBCMP,JNBCMP,NPARD,
     2         NSUBP,NATOM,X,Y,Z,NBOND,IB,JB,DD1CMP,HEAP(DDVAL),DDVALM)
         SUBLIS=ALLHP(INTEG4(NSUBP*2))
         CALL PARINT(HEAP(ATMPAF),NPARD,HEAP(SUBLIS),NSUBP)
         CALL INIPAF(HEAP(ATMPAF),NPARD)
C
C Find out with which block to continue (double window method only)
C
         IPA1=IPAR1
         IPA2=IPAR2
         IRESF=0
         IF(LWDINI) THEN
            ITER=0
            LWDINI=.FALSE.
            GOTO 500
         ENDIF
         DO II=1,NSUBP
            CALL IPART(HEAP(SUBLIS),II,IPAR1,IPAR2,HEAP(ATMPAF),
     1                 NPARD,QCALC)
            IF((IPAR1.EQ.IPA1).AND.(IPAR2.EQ.IPA2)) GOTO 500
         ENDDO
      ENDIF
 500  CONTINUE
C
C Main loop.
C
      DO WHILE((CVGMX.GT.TOLDIM).AND.(ITER.LT.ITMX))
         IF(.NOT.QDW) THEN
            ITER=ITER+1
            IF(PRNLEV.GE.2) WRITE(OUTU,553) ITER
 553  FORMAT(/' NMDIMB: Iteration number = ',I8)
            IF(INIDS.EQ.0) THEN
               INIDS=1
            ELSE
               INIDS=0
            ENDIF
            CALL PARTDS(NAT3,NPARC,ATMPAR,NPARS,ATMPAS,INIDS,NPARMX,
     1                  DDF,NFREG,CUTF1,PARDIM,NFCUT1)
C DO-THE-DIAGONALISATIONS
            ASSIGN 641 to I640 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
            GOTO 640
 641        CONTINUE
            QDIAG=.FALSE.
C DO-THE-DIAGONALISATIONS-WITH-RESIDUALS
            ASSIGN 622 TO I620 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
            GOTO 620
 622        CONTINUE
            QDIAG=.TRUE.
C SAVE-MODES
            ASSIGN 702 TO I700 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
            GOTO 700
 702        CONTINUE
C
         ELSE
            DO II=1,NSUBP
               CALL IPART(HEAP(SUBLIS),II,IPAR1,IPAR2,HEAP(ATMPAF),
     1                 NPARD,QCALC)
               IF(QCALC) THEN
                  IRESF=IRESF+1
                  ITER=ITER+1
                  IF(PRNLEV.GE.2) WRITE(OUTU,553) ITER
C DO-THE-DWIN-DIAGONALISATIONS
                  ASSIGN 661 TO I660 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
                  GOTO 660
 661              CONTINUE
               ENDIF
               IF((IRESF.EQ.SAVF).OR.(ITER.EQ.ITMX)) THEN
                  IRESF=0
                  QDIAG=.FALSE.
C DO-THE-DIAGONALISATIONS-WITH-RESIDUALS
                  ASSIGN 623 TO I620 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
                  GOTO 620
 623              CONTINUE
                  QDIAG=.TRUE.
                  IF((CVGMX.LE.TOLDIM).OR.(ITER.EQ.ITMX)) GOTO 600
C SAVE-MODES
                  ASSIGN 703 TO I700 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
                  GOTO 700
 703              CONTINUE
               ENDIF
            ENDDO
         ENDIF
      ENDDO
 600  CONTINUE
C
C SAVE-MODES
      ASSIGN 704 TO I700 ! { dg-warning "Deleted feature: ASSIGN" "Deleted feature: ASSIGN" }
      GOTO 700
 704  CONTINUE
      CALL CLEANHP(NAT3,NFREG,NPARD,NSUBP,PARDIM,DDV2,DDSS,DDVBAS,
     1             DDVAL,JSPACE,TRAROT,
     2             SCIFV1,SCIFV2,SCIFV3,SCIFV4,SCIFV6,
     3             DRATQ,ERATQ,E2RATQ,BDRATQ,INRATQ,IUPD,ATMPAF,
     4             ATMCOR,SUBLIS,LSCI,QDW,LBIG)
      RETURN
C-----------------------------------------------------------------------
C INTERNAL PROCEDURES
C-----------------------------------------------------------------------
C TO DO-THE-DIAGONALISATIONS-WITH-RESIDUALS
 620  CONTINUE
      IF(IUNRMD.LT.0) THEN
        CALL SELNMD(DDF,NFRET,CUTF1,NFC)
        N1=NFCUT1
        N2=(NFRET+6)/2
        NFCUT=MAX(N1,N2)
        IF(NFCUT*2-6 .GT. NFREG) THEN
           NFCUT=(NFREG+6)/2
           CUTF1=DDF(NFCUT)
           IF(PRNLEV.GE.2) THEN
             WRITE(OUTU,562) ITER
             WRITE(OUTU,564) CUTF1
           ENDIF
        ENDIF
      ELSE
        NFCUT=NFRET
        NFC=NFRET
      ENDIF
 562  FORMAT(/' NMDIMB: Not enough space to hold the residual vectors'/
     1       '         into DDV array during iteration ',I5)
 564  FORMAT('         Cutoff frequency is changed to ',F9.3)
C
C do reduced diagonalization with preceding eigenvectors plus
C residual vectors
C
      ISTRT=1
      ISTOP=NFCUT
      CALL CLETR(DDV,HEAP(TRAROT),NAT3,ISTRT,ISTOP,NFCUT,DDEV,DDF)
      CALL RNMTST(DDV,HEAP(DDVBAS),NAT3,DDSCR,DD1CMP,INBCMP,JNBCMP,
     2            7,NFCUT,CVGMX,NFCUT,NFC,QDIAG,LBIG,IUNMOD)
      NFSAV=NFCUT
      IF(QDIAG) THEN
         NFRET=NFCUT*2-6
         IF(PRNLEV.GE.2) WRITE(OUTU,566) NFRET
 566     FORMAT(/' NMDIMB: Diagonalization with residual vectors. '/
     1          '          Dimension of the reduced basis set'/
     2          '             before orthonormalization = ',I5)
         NFCUT=NFRET
         OLDPRN=PRNLEV
         PRNLEV=1
         CALL ORTHNM(1,NFRET,NFCUT,DDV,NAT3,LPURG,TOLER)
         PRNLEV=OLDPRN
         NFRET=NFCUT
         IF(PRNLEV.GE.2) WRITE(OUTU,568) NFRET
 568     FORMAT('             after orthonormalization  = ',I5)
         IF(LBIG) THEN
            IF(PRNLEV.GE.2) WRITE(OUTU,570) NFCUT,IUNMOD
 570        FORMAT(' NMDIMB: ',I5,' basis vectors are saved in unit',I5)
            REWIND (UNIT=IUNMOD)
            LCARD=.FALSE.
            CALL WRTNMD(LCARD,1,NFCUT,NAT3,DDV,DDSCR,DDEV,IUNMOD,AMASS)
            CALL SAVEIT(IUNMOD)
         ELSE
            CALL CPARAY(HEAP(DDVBAS),DDV,NAT3,1,NFCUT,1)
         ENDIF
         QMIX=.FALSE.
         CALL RBDG(X,Y,Z,NAT3,NDIM,NFRET,DDV,DDF,DDEV,
     1     DDSCR,HEAP(DD5),HEAP(DDSS),HEAP(DDV2),NADD,
     2     INBCMP,JNBCMP,HEAP(DDVBAS),DD1CMP,QMIX,0,0,IS3,IS4,
     3     CUTF1,NFCUT1,NFREG,HEAP(IUPD),DD1BLL,HEAP(SCIFV1),
     4     HEAP(SCIFV2),HEAP(SCIFV3),HEAP(SCIFV4),HEAP(SCIFV6),
     5     HEAP(DRATQ),HEAP(ERATQ),HEAP(E2RATQ),
     6     HEAP(BDRATQ),HEAP(INRATQ),LSCI,LBIG,IUNMOD)
         CALL SELNMD(DDF,NFRET,CUTF1,NFCUT1)
      ENDIF
      GOTO I620 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO DO-THE-DIAGONALISATIONS
 640  CONTINUE
      DO I=1,NPARC
         NFCUT1=NFRRES
         IS1=ATMPAR(1,I)
         IS2=ATMPAR(2,I)
         NDIM=(IS2-IS1+1)*3
         IF(PRNLEV.GE.2) WRITE(OUTU,573) I,IS1,IS2
 573     FORMAT(/' NMDIMB: Mixed diagonalization, part ',I5/
     1           ' NMDIMB: Block limits: ',I5,2X,I5)
         IF(NDIM+NFCUT1.GT.PARDIM) CALL WRNDIE(-3,'<NMDIMB>',
     1      'Error in dimension of block')
         NFRET=NFCUT1
         IF(NFRET.GT.NFREG) NFRET=NFREG
         CALL CLETR(DDV,HEAP(TRAROT),NAT3,1,NFCUT1,NFCUT,DDEV,DDF)
         NFCUT1=NFCUT
         CALL ADZER(DDV,1,NFCUT1,NAT3,IS1,IS2)
         NFSAV=NFCUT1
         OLDPRN=PRNLEV
         PRNLEV=1
         CALL ORTHNM(1,NFCUT1,NFCUT,DDV,NAT3,LPURG,TOLER)
         PRNLEV=OLDPRN
         CALL CPARAY(HEAP(DDVBAS),DDV,NAT3,1,NFCUT,1)
         NFRET=NDIM+NFCUT
         QMIX=.TRUE.
         CALL RBDG(X,Y,Z,NAT3,NDIM,NFRET,DDV,DDF,DDEV,
     1        DDSCR,HEAP(DD5),HEAP(DDSS),HEAP(DDV2),NADD,
     2        INBCMP,JNBCMP,HEAP(DDVBAS),DD1CMP,QMIX,IS1,IS2,IS3,IS4,
     3        CUTF1,NFCUT,NFREG,HEAP(IUPD),DD1BLL,HEAP(SCIFV1),
     4        HEAP(SCIFV2),HEAP(SCIFV3),HEAP(SCIFV4),HEAP(SCIFV6),
     5        HEAP(DRATQ),HEAP(ERATQ),HEAP(E2RATQ),
     6        HEAP(BDRATQ),HEAP(INRATQ),LSCI,LBIG,IUNMOD)
         QMIX=.FALSE.
         IF(NFCUT.GT.NFRRES) NFCUT=NFRRES
         NFCUT1=NFCUT
         NFRET=NFCUT
      ENDDO
      GOTO I640 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO DO-THE-DWIN-DIAGONALISATIONS
 660  CONTINUE
C
C Store the DDV vectors into DDVBAS
C
      NFCUT1=NFRRES
      IS1=ATMPAD(1,IPAR1)
      IS2=ATMPAD(2,IPAR1)
      IS3=ATMPAD(1,IPAR2)
      IS4=ATMPAD(2,IPAR2)
      NDIM=(IS2-IS1+IS4-IS3+2)*3
      IF(PRNLEV.GE.2) WRITE(OUTU,577) IPAR1,IPAR2,IS1,IS2,IS3,IS4
 577  FORMAT(/' NMDIMB: Mixed double window diagonalization, parts ',
     1        2I5/
     2        ' NMDIMB: Block limits: ',I5,2X,I5,4X,I5,2X,I5)
      IF(NDIM+NFCUT1.GT.PARDIM) CALL WRNDIE(-3,'<NMDIMB>',
     1      'Error in dimension of block')
      NFRET=NFCUT1
      IF(NFRET.GT.NFREG) NFRET=NFREG
C
C Prepare the DDV vectors consisting of 6 translations-rotations
C + eigenvectors from 7 to NFCUT1 + cartesian displacements vectors
C spanning the atoms from IS1 to IS2
C
      CALL CLETR(DDV,HEAP(TRAROT),NAT3,1,NFCUT1,NFCUT,DDEV,DDF)
      NFCUT1=NFCUT
      NFSAV=NFCUT1
      CALL ADZERD(DDV,1,NFCUT1,NAT3,IS1,IS2,IS3,IS4)
      OLDPRN=PRNLEV
      PRNLEV=1
      CALL ORTHNM(1,NFCUT1,NFCUT,DDV,NAT3,LPURG,TOLER)
      PRNLEV=OLDPRN
      CALL CPARAY(HEAP(DDVBAS),DDV,NAT3,1,NFCUT,1)
C
      NFRET=NDIM+NFCUT
      QMIX=.TRUE.
      CALL RBDG(X,Y,Z,NAT3,NDIM,NFRET,DDV,DDF,DDEV,
     1     DDSCR,HEAP(DD5),HEAP(DDSS),HEAP(DDV2),NADD,
     2     INBCMP,JNBCMP,HEAP(DDVBAS),DD1CMP,QMIX,IS1,IS2,IS3,IS4,
     3     CUTF1,NFCUT,NFREG,HEAP(IUPD),DD1BLL,HEAP(SCIFV1),
     4     HEAP(SCIFV2),HEAP(SCIFV3),HEAP(SCIFV4),HEAP(SCIFV6),
     5     HEAP(DRATQ),HEAP(ERATQ),HEAP(E2RATQ),
     6     HEAP(BDRATQ),HEAP(INRATQ),LSCI,LBIG,IUNMOD)
      QMIX=.FALSE.
C
      IF(NFCUT.GT.NFRRES) NFCUT=NFRRES
      NFCUT1=NFCUT
      NFRET=NFCUT
      GOTO I660 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO SAVE-MODES
 700  CONTINUE
      IF(PRNLEV.GE.2) WRITE(OUTU,583) IUNMOD
 583  FORMAT(/' NMDIMB: Saving the eigenvalues and eigenvectors to unit'
     1       ,I4)
      REWIND (UNIT=IUNMOD)
      ISTRT=1
      ISTOP=NFSAV
      LCARD=.FALSE.
      IF(PRNLEV.GE.2) WRITE(OUTU,585) NFSAV,IUNMOD
 585  FORMAT(' NMDIMB: ',I5,' modes are saved in unit',I5)
      CALL WRTNMD(LCARD,ISTRT,ISTOP,NAT3,DDV,DDSCR,DDEV,IUNMOD,
     1            AMASS)
      CALL SAVEIT(IUNMOD)
      GOTO I700 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO ALLOCATE-SPACE-FOR-DIAGONALIZATION
 720  CONTINUE
      DDV2=ALLHP(IREAL8((PARDIM+3)*(PARDIM+3)))
      JSPACE=IREAL8((PARDIM+4))*8
      JSP=IREAL8(((PARDIM+3)*(PARDIM+4))/2)
      JSPACE=JSPACE+JSP
      DDSS=ALLHP(JSPACE)
      DD5=DDSS+JSPACE-JSP
      GOTO I720 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO ALLOCATE-SPACE-FOR-REDUCED-BASIS
 760  CONTINUE
      IF(LBIG) THEN
         DDVBAS=ALLHP(IREAL8(NAT3))
      ELSE
         DDVBAS=ALLHP(IREAL8(NFREG*NAT3))
      ENDIF
      GOTO I760 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO ALLOCATE-SPACE-FOR-TRANSROT-VECTORS
 800  CONTINUE
      TRAROT=ALLHP(IREAL8(6*NAT3))
      GOTO I800 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO ALLOCATE-SPACE-FOR-LSCI
 840  CONTINUE
      SCIFV1=ALLHP(IREAL8(PARDIM+3))
      SCIFV2=ALLHP(IREAL8(PARDIM+3))
      SCIFV3=ALLHP(IREAL8(PARDIM+3))
      SCIFV4=ALLHP(IREAL8(PARDIM+3))
      SCIFV6=ALLHP(IREAL8(PARDIM+3))
      DRATQ=ALLHP(IREAL8(PARDIM+3))
      ERATQ=ALLHP(IREAL8(PARDIM+3))
      E2RATQ=ALLHP(IREAL8(PARDIM+3))
      BDRATQ=ALLHP(IREAL8(PARDIM+3))
      INRATQ=ALLHP(INTEG4(PARDIM+3))
      GOTO I840 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO ALLOCATE-DUMMY-SPACE-FOR-LSCI
 880  CONTINUE
      SCIFV1=ALLHP(IREAL8(2))
      SCIFV2=ALLHP(IREAL8(2))
      SCIFV3=ALLHP(IREAL8(2))
      SCIFV4=ALLHP(IREAL8(2))
      SCIFV6=ALLHP(IREAL8(2))
      DRATQ=ALLHP(IREAL8(2))
      ERATQ=ALLHP(IREAL8(2))
      E2RATQ=ALLHP(IREAL8(2))
      BDRATQ=ALLHP(IREAL8(2))
      INRATQ=ALLHP(INTEG4(2))
      GOTO I880 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C
C-----------------------------------------------------------------------
C TO ALLOCATE-SPACE-FOR-OTHER-ARRAYS
 920  CONTINUE
      IUPD=ALLHP(INTEG4(PARDIM+3))
      GOTO I920 ! { dg-warning "Deleted feature: Assigned" "Assigned GO TO" }
C.##ELSE
C.##ENDIF
      END
