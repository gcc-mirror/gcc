! PR middle-end/20030
! we were messing up the access in LSAME for
! the character agruments.
      program foo
      character*1 a1, a2, b
      a1='A'
      a2='A'
      b='B'
      x = LSAME(a1,a2)
      if ( x.ne.1 ) then
        call abort  ();
      endif
      end

      logical function LSAME( CA, CB )
      character CA, CB
      integer   INTA, INTB
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
      LSAME = INTA.EQ.INTB
      end 
