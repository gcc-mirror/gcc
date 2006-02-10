! PR 14771
! { dg-do run }
! Originally contributed by Walt Brainerd, modified for the testsuite
      PROGRAM fc107

! Submitted by Walt Brainerd, The Fortran Company
! GNU Fortran 95 (GCC 4.1.0 20050322 (experimental))
! Windows XP

! Return value should be 3

      INTEGER I, J, M(2), N(2)
      integer, pointer :: k
      integer, target :: l
      INTEGER TRYME

      interface
        FUNCTION TRYyou(RTNME,HITME)
          INTEGER RTNME(2),HITME(2), tryyou(2)
        END function tryyou
      end interface

      m = 7
      l = 5
      I = 3
      k => l

      j = tryme((i),i)
      if (j .ne. 3) call abort ()

      j = tryme((k),k)
      if (j .ne. 5) call abort ()

      n = tryyou((m),m)
      if (any(n .ne. 7)) call abort ()
      END

      INTEGER FUNCTION TRYME(RTNME,HITME)
      INTEGER RTNME,HITME
      HITME = 999
      TRYME = RTNME
      END

      FUNCTION TRYyou(RTNME,HITME)
      INTEGER RTNME(2),HITME(2), tryyou(2)
      HITME = 999
      TRYyou = RTNME
      END
