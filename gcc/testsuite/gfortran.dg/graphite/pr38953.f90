! { dg-options "-O3  -fgraphite-identity" }

      MODULE MAIN1
      INTEGER , PARAMETER :: IFMAX = 40 , IKN = 85 , ISTRG = 132 ,      &
     &                       IERRN = 170 , ILEN_FLD = 80
      CHARACTER PATH*2 , PPATH*2 , KEYWRD*8 , PKEYWD*8 , KEYWD*8 ,      &
     &          KTYPE*5 , RUNST*1
      DIMENSION FIELD(IFMAX) , KEYWD(IKN) , RUNST(ISTRG)
      LOGICAL :: DFAULT , CONC , DEPOS , DDEP , WDEP , RURAL , URBAN ,  &
     &        GRDRIS , NOSTD , NOBID , CLMPRO , MSGPRO , PERIOD ,       &
     &            OLM=.FALSE.    
      INTEGER :: NSRC , NREC , NGRP , NQF,                              &
     &           NARC , NOLM
      CHARACTER NETID*8 , NETIDT*8 , PNETID*8 , NTID*8 , NTTYP*8 ,      &
     &          RECTYP*2 , PXSOID*8 , PESOID*8 , ARCID*8
      ALLOCATABLE ::NETID(:) , RECTYP(:) , NTID(:) , NTTYP(:) , ARCID(:)
      DATA (KEYWD(I),I=1,IKN)/'STARTING' , 'FINISHED' , 'TITLEONE' ,    &
     &      'TITLETWO' , 'MODELOPT' , 'AVERTIME' , 'POLLUTID' ,         &
     &      'HALFLIFE' , 'DCAYCOEF' , 'DEBUGOPT' , 'ELEVUNIT' ,         &
     &      'FLAGPOLE' , 'RUNORNOT' , 'EVENTFIL' , 'SAVEFILE' ,         &
     &      'INITFILE' , 'MULTYEAR' , 'ERRORFIL' , 'GASDEPDF' ,         &
     &      'GDSEASON' , 'GASDEPVD' , 'GDLANUSE' , 'EVENTFIL' ,         &
     &      'URBANOPT' , 'METHOD_2' , 'LOCATION' , 'SRCPARAM' ,         &
     &      'BUILDHGT' , 'BUILDWID' , 'BUILDLEN' , 'XBADJ   ' ,         &
     &      'YBADJ   ' , 'EMISFACT' , 'EMISUNIT' , 'PARTDIAM' ,         &
     &      'MASSFRAX' , 'PARTDENS' , '        ' , '        ' ,         &
     &      '        ' , 'CONCUNIT' , 'DEPOUNIT' , 'HOUREMIS' ,         &
     &      'GASDEPOS' , 'URBANSRC' , 'EVENTPER' , 'EVENTLOC' ,         &
     &      'SRCGROUP' , 'GRIDCART' , 'GRIDPOLR' , 'DISCCART' ,         &
     &      'DISCPOLR' , 'SURFFILE' , 'PROFFILE' , 'PROFBASE' ,         &
     &      '        ' , 'SURFDATA' , 'UAIRDATA' , 'SITEDATA' ,         &
     &      'STARTEND' , 'DAYRANGE' , 'WDROTATE' , 'DTHETADZ' ,         &
     &      'WINDCATS' , 'RECTABLE' , 'MAXTABLE' , 'DAYTABLE' ,         &
     &      'MAXIFILE' , 'POSTFILE' , 'PLOTFILE' , 'TOXXFILE' ,         &
     &      'EVENTOUT' , 'INCLUDED' , 'SCIMBYHR' , 'SEASONHR' ,         &
     &      'AREAVERT' , 'PARTSIZE' , 'RANKFILE' , 'EVALCART' ,         &
     &      'EVALFILE' , 'NO2EQUIL' , 'OZONEVAL' , 'OZONEFIL' ,         &
     &      'NO2RATIO' , 'OLMGROUP'/
      DIMENSION RESTAB(9,6,5) , STAB(9)
      DATA (((RESTAB(I,J,K),I=1,9),J=1,6),K=1,5)/1.E07 , 60. , 120. ,   &
     &      100. , 200. , 150. , 1.E07 , 1.E07 , 80. , 1.E07 , 2000. ,  &
     &      2000. , 2000. , 2000. , 2000. , 1.E07 , 1.E07 , 2500. ,     &
     &      1.E07 , 1000. , 1000. , 1000. , 2000. , 2000. , 1.E07 ,     &
     &      1.E07 , 1000. , 100. , 200. , 100. , 2000. , 100. , 1500. , &
     &      0. , 0. , 300. , 400. , 150. , 350. , 300. , 500. , 450. ,  &
     &      0. , 1000. , 0. , 300. , 150. , 200. , 200. , 300. , 300. , &
     &      2000. , 400. , 1000. , 1.E07 , 1.E07 , 1.E07 , 350. ,       &
     &      1.E07 , 700. , 1.E07 , 1.E07 , 1.E07 , 1.E07 , 6500. ,      &
     &      6500. , 3000. , 2000. , 2000. , 1.E07 , 1.E07 , 6500. ,     &
     &      1.E07 , 400. , 300. , 500. , 600. , 1000. , 1.E07 , 1.E07 , &
     &      300. , 100. , 150. , 100. , 1700. , 100. , 1200. , 0. , 0. ,&
     &      200. , 400. , 200. , 350. , 300. , 500. , 450. , 0. ,       &
     &      1000. , 0. , 300. , 150. , 200. , 200. , 300. , 300. ,      &
     &      2000. , 400. , 800. , 1.E07 , 1.E07 , 1.E07 , 500. , 1.E07 ,&
     &      1000. , 1.E07 , 1.E07 , 1.E07 , 1.E07 , 1.E07 , 9000. ,     &
     &      6000. , 2000. , 2000. , 1.E07 , 1.E07 , 9000. , 1.E07 ,     &
     &      1.E07 , 400. , 600. , 800. , 1600. , 1.E07 , 1.E07 , 800. , &
     &      100. , 0. , 100. , 1500. , 100. , 1000. , 0. , 0. , 100. ,  &
     &      400. , 150. , 350. , 300. , 500. , 450. , 0. , 0. , 1000. , &
     &      300. , 150. , 200. , 200. , 300. , 300. , 2000. , 400. ,    &
     &      1000. , 1.E07 , 1.E07 , 1.E07 , 800. , 1.E07 , 1600. ,      &
     &      1.E07 , 1.E07 , 1.E07 , 1.E07 , 1.E07 , 1.E07 , 400. ,      &
     &      1.E07 , 800. , 1.E07 , 1.E07 , 9000. , 1.E07 , 2000. ,      &
     &      1000. , 600. , 2000. , 1200. , 1.E07 , 1.E07 , 800. , 100. ,&
     &      0. , 10. , 1500. , 100. , 1000. , 0. , 0. , 50. , 100. ,    &
     &      100. , 100. , 100. , 200. , 200. , 0. , 1000. , 100. ,      &
     &      600. , 3500. , 3500. , 3500. , 500. , 500. , 2000. , 400. , &
     &      3500. , 1.E07 , 100. , 120. , 100. , 200. , 150. , 1.E07 ,  &
     &      1.E07 , 80. , 1.E07 , 2000. , 2000. , 1500. , 2000. ,       &
     &      2000. , 1.E07 , 1.E07 , 2000. , 1.E07 , 1000. , 250. ,      &
     &      350. , 500. , 700. , 1.E07 , 1.E07 , 300. , 100. , 50. ,    &
     &      80. , 1500. , 100. , 1000. , 0. , 0. , 200. , 500. , 150. , &
     &      350. , 300. , 500. , 450. , 0. , 1000. , 0. , 300. , 150. , &
     &      200. , 200. , 300. , 300. , 2000. , 400. , 1000./
      END
      SUBROUTINE SHAVE
      USE MAIN1
      IF ( PERIOD ) THEN
 9020    FORMAT ('(''*'',8X,''X'',13X,''Y'',4X,',I1,                    &
     &'(2X,3A4),4X,''ZELEV'',   4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,&
     &_______  ________  ________'')')
      ENDIF
      DO IGRP = 1 , NUMGRP
         IF ( IANPST(IGRP).EQ.1 ) THEN
            IF ( IANFRM(IGRP).EQ.0 ) THEN
               DO IREC = 1 , NUMREC
               ENDDO
            ENDIF
            DO IREC = 1 , NUMREC
               IF ( RECTYP(IREC).EQ.'DC' ) THEN
                  WRITE (IOUNIT,9082) SRCID(ISRF) , SRCTYP(ISRF) ,      &
     &                                AXS(ISRF) , AYS(ISRF) , AZS(ISRF) &
     &                                , (J,AXR(IREC+J-1),AYR(IREC+J-1), &
     &                                HCLMSG(IREC+J-1,IHNUM,IGRP,IAVE,  &
     &                                ITYP),J=1,36)
 9082             FORMAT (' BOUNDARY RECEPTOR NETWORK OF SOURCE ID: ',  &
     &                    18(2(1X,I4,3X,F10.2,', ',F10.2,',',F13.5,A1,  &
     &                    '(',I8.8,')',7X),/),/)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      END
      USE MAIN1
      IF ( ICOUNT.NE.0 .AND. JCOUNT.NE.0 ) THEN
         DO J = 1 , JCOUNT
            DO I = 1 , ICOUNT
               IF ( ISET.GT.NREC ) THEN
                  GOTO 999
               ENDIF
            ENDDO
         ENDDO
      ENDIF
 999  CONTINUE
      END
! { dg-final { cleanup-modules "main1" } }
