! { dg-do compile }
! PR fortran/26551
      SUBROUTINE SUB()
      CALL SUB() ! { dg-error "cannot call itself, as it is not RECURSIVE" }
      END SUBROUTINE

      FUNCTION FUNC() RESULT (FOO)
      INTEGER FOO
      FOO = FUNC() ! { dg-error "cannot call itself, as it is not RECURSIVE" }
      END FUNCTION

      SUBROUTINE SUB2()
      ENTRY ENT2()
      CALL ENT2() ! { dg-error "is not declared as RECURSIVE" }
      END SUBROUTINE

      function func2()
      integer func2
      func2 = 42
      return
      entry c() result (foo)
      foo = b() ! { dg-error "is not declared as RECURSIVE" }
      return
      entry b() result (bar)
      bar = 12
      return
      end function
