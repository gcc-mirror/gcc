! { dg-lto-do link }
! { dg-extra-ld-options "-r -nostdlib -finline-functions -flinker-output=nolto-rel" }

SUBROUTINE int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, Element, VarName, Data, code )
  CALL int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, 1, &
                             DataHandle, DummyData, DummyCount, code )
END SUBROUTINE int_gen_ti_header_char

