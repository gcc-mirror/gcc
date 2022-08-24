! { dg-do compile }

      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,           &
     &                   BETA, Y, INCY )
      LOGICAL            LSAME
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.                            &
     &         .NOT.LSAME( TRANS, 'C' )      )THEN
      END IF
      END
      subroutine evlrnf (ptrs0t, nclsm, prnf0t) 
      real, dimension (1:nclsm,1:nclsm), intent (in) :: ptrs0t
      real, dimension (1:nclsm,1:nclsm), intent (out):: prnf0t
      real, allocatable, dimension (:,:) :: utrsft ! probas up
      real, allocatable, dimension (:,:) :: dtrsft ! probas down
      real, allocatable, dimension (:,:) :: xwrkt ! matrice
      do icls = 1, nclsm
         do ival = ipic - 1, 1, -1
            xwrkt = trs2a2 (ival, ipic, utrsft, dtrsft, ncls)
         enddo
      enddo
      contains
      function trs2a2 (j, k, u, d, m)
      real, dimension (1:m,1:m) :: trs2a2  ! resultat
      real, dimension (1:m,1:m) :: u, d    ! matrices utrsft, dtrsft
      end function trs2a2
      end
      program rnflow
      integer, parameter :: ncls  =     256 ! nombre de classes
      integer, dimension (1:ncls,1:ncls) :: mrnftt ! matrice theorique
      real, dimension (1:ncls,1:ncls)    :: ptrst  ! matrice Markov
      real, dimension (1:ncls,1:ncls)    :: prnft  ! matrice Rainflow
      call evlrnf (ptrst, ncls, prnft)
      mrnftt = nint (real (nsim) * real (npic) * prnft)
      call cmpmat (mrnftt, mrnfst)
      end program rnflow
