! { dg-do compile }
! { dg-require-effective-target vect_double }

      function trs2a2 (j, k, u, d, m)
!      matrice de transition intermediaire, partant de k sans descendre
!      sous j. R = IjU(I-Ik)DIj, avec Ii = deltajj, j >= i.
!      alternative: trs2a2 = 0
!                   trs2a2 (j:k-1, j:k-1) = matmul (utrsft (j:k-1,j:k-1),
!                                                   dtrsft (j:k-1,j:k-1))
!
      real, dimension (1:m,1:m) :: trs2a2  ! resultat
      real, dimension (1:m,1:m) :: u, d    ! matrices utrsft, dtrsft
      integer, intent (in)      :: j, k, m ! niveaux vallee pic
!
!##### following line replaced by Prentice to make less system dependent
!      real (kind = kind (1.0d0)) :: dtmp
      real (kind = selected_real_kind (10,50)) :: dtmp
!
      trs2a2 = 0.0
      do iclw1 = j, k - 1
         do iclw2 = j, k - 1
            dtmp = 0.0d0
            do iclww = j, k - 1
               dtmp = dtmp + u (iclw1, iclww) * d (iclww, iclw2)
            enddo
            trs2a2 (iclw1, iclw2) = dtmp
         enddo
      enddo
      return
      end function trs2a2

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } }
! { dg-final { cleanup-tree-dump "vect" } }
