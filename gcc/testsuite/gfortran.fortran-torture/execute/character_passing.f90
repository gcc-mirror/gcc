! PR middle-end/20030
! we were messing up the access in LSAME for
! the character arguments.
      program foo
      character*1 a1, a2, b
      logical LSAME, x
      a1='A'
      a2='A'
      b='B'
      x = LSAME(a1,a2)
      if ( .not. x ) then
        STOP 1;
      endif
      end

      logical function LSAME( CA, CB )
      character CA, CB
      integer   INTA, INTB
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
      LSAME = INTA.EQ.INTB
      end 
