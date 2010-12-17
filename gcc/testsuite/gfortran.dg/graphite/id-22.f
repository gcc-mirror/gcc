! { dg-options "-O3 -ffast-math" }

      COMMON /NONEQ / UNZOR
      DO ITS = 1, NTS
        DO JATOM = 1, NAT
          IF(IEF.EQ.5.OR.IEF.EQ.8)
     *       UNZOR = UNZOR + 8
        ENDDO
      ENDDO
      END
