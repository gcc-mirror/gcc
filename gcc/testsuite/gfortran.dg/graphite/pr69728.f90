! { dg-do compile }
! { dg-options "-O3 -floop-nest-optimize" }
SUBROUTINE rk_addtend_dry ( t_tend, t_tendf, t_save, rk_step, &
                            h_diabatic, mut, msft, ide, jde,  &
                            ims,ime, jms,jme, kms,kme,        &
                            its,ite, jts,jte, kts,kte)
   IMPLICIT NONE
   INTEGER ,  INTENT(IN   ) :: ide, jde, ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte
   INTEGER ,  INTENT(IN   ) :: rk_step
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ), &
       INTENT(INOUT) :: t_tend, t_tendf
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , &
       INTENT(IN   ) ::  t_save, h_diabatic
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut, msft
   INTEGER :: i, j, k
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     IF(rk_step == 1)t_tendf(i,k,j) = t_tendf(i,k,j) +  t_save(i,k,j)
      t_tend(i,k,j) =  t_tend(i,k,j) +  t_tendf(i,k,j)/msft(i,j)  &
                                     +  mut(i,j)*h_diabatic(i,k,j)/msft(i,j)
   ENDDO
   ENDDO
   ENDDO
END SUBROUTINE rk_addtend_dry
