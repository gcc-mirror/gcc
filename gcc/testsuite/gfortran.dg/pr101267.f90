! { dg-do compile }
! { dg-options "-Ofast" }
! { dg-additional-options "-march=znver2" { target x86_64-*-* i?86-*-* } }
   SUBROUTINE sfddagd( regime, znt,ite ,jte )
   REAL, DIMENSION( ime, IN) :: regime, znt
   REAL, DIMENSION( ite, jte) :: wndcor_u 
   LOGICAL wrf_dm_on_monitor
   IF( int4 == 1 ) THEN
     DO j=jts,jtf
      DO i=itsu,itf
       reg =   regime(i,  j) 
       IF( reg > 10.0 ) THEN
         znt0 = znt(i-1,  j) + znt(i,  j) 
         IF( znt0 <= 0.2) THEN
           wndcor_u(i,j) = 0.2
         ENDIF
       ENDIF
      ENDDO
     ENDDO
     IF ( wrf_dm_on_monitor()) THEN
     ENDIF
   ENDIF
   END
