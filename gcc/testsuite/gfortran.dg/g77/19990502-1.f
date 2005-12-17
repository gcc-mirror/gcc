c { dg-do compile }
      SUBROUTINE G(IGAMS,IWRK,NADC,NCellsInY)
      INTEGER(kind=2) IGAMS(2,NADC)
      in = 1
      do while (in.le.nadc.and.IGAMS(2,in).le.in)
      enddo
      END
