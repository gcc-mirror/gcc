c { dg-do compile }
c PR fortran/67758
c
c Check the absence of ICE after emitting the error message
c
c Contributed by Ilya Enkovich <ienkovich@gcc.gnu.org>

      COMMON /FMCOM / X(80 000 000)
      CALL T(XX(A))
      COMMON /FMCOM / XX(80 000 000) ! { dg-error "Unexpected COMMON" }
      END
