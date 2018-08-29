C     fixed-form literal character constant with continuation line padding test
C     PR fortran/25486
      program a
      character(len=90) c
      character(90) :: fil
c A tab is between 8 and 9.
      c = '1234567
     &8	9'
      write(fil,'(a)') c
#ifdef LL_NONE
      if(fil.ne. "12345678	9")
     &  STOP 1
#else
      if(fil.ne.
     &"1234567                                                      8	9"
     &)
     &  STOP 2
#endif
      end

