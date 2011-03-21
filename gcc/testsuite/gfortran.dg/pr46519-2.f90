! { dg-do compile { target i?86-*-* x86_64-*-* } }
! { dg-options "-O3 -mavx -mvzeroupper -mtune=generic -dp" }

  SUBROUTINE func(kts, kte, qrz, qiz, rho)
  IMPLICIT NONE
  INTEGER, INTENT(IN)               :: kts, kte
  REAL,    DIMENSION(kts:kte), INTENT(INOUT) :: qrz, qiz, rho
  INTEGER                              :: k
  REAL, DIMENSION(kts:kte)    ::  praci, vtiold
  REAL                          :: fluxout
  INTEGER                       :: min_q, max_q, var
  do k=kts,kte
    praci(k)=0.0
  enddo
  min_q=kte
  max_q=kts-1
  DO var=1,20
    do k=max_q,min_q,-1
       fluxout=rho(k)*qrz(k)
    enddo
    qrz(min_q-1)=qrz(min_q-1)+fluxout
  ENDDO
  DO var=1,20
    do k=kts,kte-1
      vtiold(k)= (rho(k))**0.16
    enddo
  ENDDO
  STOP
  END SUBROUTINE func

! { dg-final { scan-assembler "avx_vzeroupper" } }
