! { dg-do compile }
! Check for PR 27478
      FUNCTION X()
      ENTRY X1
      IF (X .GT. 0) CALL FOO(X)
      IF (Y .GT. 0) CALL FOO(Y)
      END

      FUNCTION TSL(PIN)
      ENTRY TSL1(PIN)
      IF (DBLE(TSL) .GT. PIN) TSL = 705.47
      TSL= PPP(TSL)
      END
