! { dg-do compile }
! { dg-options "-O -fbounds-check -w" }
MODULE kinds
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND ( 14, 200 )
  INTEGER, DIMENSION(:), ALLOCATABLE     :: nco,ncoset,nso,nsoset
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: co,coset
END MODULE kinds
MODULE ai_moments
  USE kinds
CONTAINS
  SUBROUTINE cossin(la_max,npgfa,zeta,rpgfa,la_min,&
                    lb_max,npgfb,zetb,rpgfb,lb_min,&
                    rac,rbc,kvec,cosab,sinab)
    REAL(KIND=dp), DIMENSION(ncoset(la_max),&
      ncoset(lb_max))                        :: sc, ss
    DO ipgf=1,npgfa
      DO jpgf=1,npgfb
        IF (la_max > 0) THEN
          DO la=2,la_max
            DO ax=2,la
              DO ay=0,la-ax
                sc(coset(ax,ay,az),1) = rap(1)*sc(coset(ax-1,ay,az),1) +&
                              f2 *          kvec(1)*ss(coset(ax-1,ay,az),1)
                ss(coset(ax,ay,az),1) = rap(1)*ss(coset(ax-1,ay,az),1) +&
                              f2 *          kvec(1)*sc(coset(ax-1,ay,az),1)
              END DO
            END DO
          END DO
          IF (lb_max > 0) THEN
            DO lb=2,lb_max
              ss(1,coset(0,0,lb)) = rbp(3)*ss(1,coset(0,0,lb-1)) +&
                           f2 *         kvec(3)*sc(1,coset(0,0,lb-1))
              DO bx=2,lb
                DO by=0,lb-bx
                  ss(1,coset(bx,by,bz)) = rbp(1)*ss(1,coset(bx-1,by,bz)) +&
                               f2 *           kvec(1)*sc(1,coset(bx-1,by,bz))
                END DO
              END DO
            END DO
          END IF
        END IF
       DO j=ncoset(lb_min-1)+1,ncoset(lb_max)
        END DO
      END DO
    END DO
  END SUBROUTINE cossin
  SUBROUTINE moment(la_max,npgfa,zeta,rpgfa,la_min,&
                    lb_max,npgfb,zetb,rpgfb,&
                    lc_max,rac,rbc,mab)
    REAL(KIND=dp), DIMENSION(:), INTENT(IN)  :: zeta, rpgfa
    REAL(KIND=dp), DIMENSION(:), INTENT(IN)  :: zetb, rpgfb
    REAL(KIND=dp), DIMENSION(:, :, :), &
      INTENT(INOUT)                          :: mab
    REAL(KIND=dp), DIMENSION(3)              :: rab, rap, rbp, rpc
    REAL(KIND=dp), DIMENSION(ncoset(la_max),&
      ncoset(lb_max), ncoset(lc_max))        :: s
    DO ipgf=1,npgfa
      DO jpgf=1,npgfb
        IF (rpgfa(ipgf) + rpgfb(jpgf) < dab) THEN
          DO k=1, ncoset(lc_max)-1
            DO j=nb+1,nb+ncoset(lb_max)
              DO i=na+1,na+ncoset(la_max)
                mab(i,j,k) = 0.0_dp
              END DO
            END DO
          END DO
        END IF
        rpc = zetp*(zeta(ipgf)*rac+zetb(jpgf)*rbc)
        DO l=2, ncoset(lc_max)
          lx = indco(1,l)
          l2 = 0
          IF ( lz > 0 ) THEN
            IF ( lz > 1 ) l2 = coset(lx,ly,lz-2)
          ELSE IF ( ly > 0 ) THEN
            IF ( ly > 1 ) l2 = coset(lx,ly-2,lz)
            IF ( lx > 1 ) l2 = coset(lx-2,ly,lz)
          END IF
          s(1,1,l) = rpc(i)*s(1,1,l1)
          IF ( l2 > 0 ) s(1,1,l) = s(1,1,l) + f2*REAL(ni,dp)*s(1,1,l2)
        END DO
        DO l = 1, ncoset(lc_max)
          IF ( lx > 0 ) THEN 
            lx1 = coset(lx-1,ly,lz)
          END IF
          IF ( ly > 0 ) THEN 
            ly1 = coset(lx,ly-1,lz)
          END IF
          IF (la_max > 0) THEN
            DO la=2,la_max
              IF ( lz1 > 0 ) s(coset(0,0,la),1,l) = s(coset(0,0,la),1,l) + &
                             f2z*s(coset(0,0,la-1),1,lz1)
              IF ( ly1 > 0 ) s(coset(0,1,az),1,l) = s(coset(0,1,az),1,l) + &
                             f2y*s(coset(0,0,az),1,ly1)
              DO ay=2,la
                s(coset(0,ay,az),1,l) = rap(2)*s(coset(0,ay-1,az),1,l) +&
                                       f2*REAL(ay-1,dp)*s(coset(0,ay-2,az),1,l)
                IF ( ly1 > 0 ) s(coset(0,ay,az),1,l) = s(coset(0,ay,az),1,l) + &
                             f2y*s(coset(0,ay-1,az),1,ly1)
              END DO
              DO ay=0,la-1
                IF ( lx1 > 0 ) s(coset(1,ay,az),1,l) = s(coset(1,ay,az),1,l) + &
                             f2x*s(coset(0,ay,az),1,lx1)
              END DO
              DO ax=2,la
                DO ay=0,la-ax
                  s(coset(ax,ay,az),1,l) = rap(1)*s(coset(ax-1,ay,az),1,l) +&
                                          f3*s(coset(ax-2,ay,az),1,l) 
                  IF ( lx1 > 0 ) s(coset(ax,ay,az),1,l) = s(coset(ax,ay,az),1,l) + &
                                 f2x*s(coset(ax-1,ay,az),1,lx1)
                END DO
              END DO
            END DO
            IF (lb_max > 0) THEN
              DO j=2,ncoset(lb_max)
                DO i=1,ncoset(la_max)
                  s(i,j,l) = 0.0_dp
                END DO
              END DO
              DO la=la_start,la_max-1
                DO ax=0,la
                  DO ay=0,la-ax
                    s(coset(ax,ay,az),2,l) = s(coset(ax+1,ay,az),1,l) -&
                                           rab(1)*s(coset(ax,ay,az),1,l)
                    s(coset(ax,ay,az),4,l) = s(coset(ax,ay,az+1),1,l) -&
                                           rab(3)*s(coset(ax,ay,az),1,l)
                  END DO
                END DO
              END DO
              DO ax=0,la_max
                DO ay=0,la_max-ax
                  IF (ax == 0) THEN
                    s(coset(ax,ay,az),2,l) = rbp(1)*s(coset(ax,ay,az),1,l)
                  ELSE
                    s(coset(ax,ay,az),2,l) = rbp(1)*s(coset(ax,ay,az),1,l) +&
                                            fx*s(coset(ax-1,ay,az),1,l)
                  END IF
                  IF (lx1 > 0) s(coset(ax,ay,az),2,l) = s(coset(ax,ay,az),2,l) +&
                        f2x*s(coset(ax,ay,az),1,lx1)
                  IF (ay == 0) THEN
                    s(coset(ax,ay,az),3,l) = rbp(2)*s(coset(ax,ay,az),1,l)
                  ELSE
                    s(coset(ax,ay,az),3,l) = rbp(2)*s(coset(ax,ay,az),1,l) +&
                                            fy*s(coset(ax,ay-1,az),1,l)
                  END IF
                  IF (ly1 > 0) s(coset(ax,ay,az),3,l) = s(coset(ax,ay,az),3,l) +&
                        f2y*s(coset(ax,ay,az),1,ly1)
                  IF (az == 0) THEN
                    s(coset(ax,ay,az),4,l) = rbp(3)*s(coset(ax,ay,az),1,l)
                  ELSE
                    s(coset(ax,ay,az),4,l) = rbp(3)*s(coset(ax,ay,az),1,l) +&
                                            fz*s(coset(ax,ay,az-1),1,l)
                  END IF
                  IF (lz1 > 0) s(coset(ax,ay,az),4,l) = s(coset(ax,ay,az),4,l) +&
                        f2z*s(coset(ax,ay,az),1,lz1)
                END DO
              END DO
              DO lb=2,lb_max
                DO la=la_start,la_max-1
                  DO ax=0,la
                    DO ay=0,la-ax
                      s(coset(ax,ay,az),coset(0,0,lb),l) =&
                        rab(3)*s(coset(ax,ay,az),coset(0,0,lb-1),l)
                      DO bx=1,lb
                        DO by=0,lb-bx
                          s(coset(ax,ay,az),coset(bx,by,bz),l) =&
                            rab(1)*s(coset(ax,ay,az),coset(bx-1,by,bz),l)
                        END DO
                      END DO
                    END DO
                  END DO
                END DO
                DO ax=0,la_max
                  DO ay=0,la_max-ax
                    IF (az == 0) THEN
                      s(coset(ax,ay,az),coset(0,0,lb),l) =&
                        rbp(3)*s(coset(ax,ay,az),coset(0,0,lb-1),l) +&
                        f3*s(coset(ax,ay,az),coset(0,0,lb-2),l)
                    END IF
                    IF (lz1 > 0) s(coset(ax,ay,az),coset(0,0,lb),l) =&
                                 f2z*s(coset(ax,ay,az),coset(0,0,lb-1),lz1)
                    IF (ay == 0) THEN
                      IF (ly1 > 0) s(coset(ax,ay,az),coset(0,1,bz),l) =&
                                 f2y*s(coset(ax,ay,az),coset(0,0,bz),ly1)
                      DO by=2,lb
                        s(coset(ax,ay,az),coset(0,by,bz),l) =&
                          f3*s(coset(ax,ay,az),coset(0,by-2,bz),l)
                        IF (ly1 > 0) s(coset(ax,ay,az),coset(0,by,bz),l) =&
                                 f2y*s(coset(ax,ay,az),coset(0,by-1,bz),ly1)
                      END DO
                      s(coset(ax,ay,az),coset(0,1,bz),l) =&
                        fy*s(coset(ax,ay-1,az),coset(0,0,bz),l)
                    END IF
                    IF (ax == 0) THEN
                      DO by=0,lb-1
                        IF (lx1 > 0) s(coset(ax,ay,az),coset(1,by,bz),l) =&
                                 f2x*s(coset(ax,ay,az),coset(0,by,bz),lx1)
                      END DO
                      DO bx=2,lb
                        DO by=0,lb-bx
                          s(coset(ax,ay,az),coset(bx,by,bz),l) =&
                            f3*s(coset(ax,ay,az),coset(bx-2,by,bz),l)
                          IF (lx1 > 0) s(coset(ax,ay,az),coset(bx,by,bz),l) =&
                                 f2x*s(coset(ax,ay,az),coset(bx-1,by,bz),lx1)
                        END DO
                      END DO
                      DO by=0,lb-1
                        IF (lx1 > 0) s(coset(ax,ay,az),coset(1,by,bz),l) =&
                                 f2x*s(coset(ax,ay,az),coset(0,by,bz),lx1)
                      END DO
                      DO bx=2,lb
                        DO by=0,lb-bx
                          s(coset(ax,ay,az),coset(bx,by,bz),l) =&
                            f3*s(coset(ax,ay,az),coset(bx-2,by,bz),l)
                          IF (lx1 > 0) s(coset(ax,ay,az),coset(bx,by,bz),l) =&
                                 f2x*s(coset(ax,ay,az),coset(bx-1,by,bz),lx1)
                        END DO
                      END DO
                    END IF
                  END DO
                END DO
              END DO
            END IF
            IF (lb_max > 0) THEN
              DO lb=2,lb_max
                IF (lz1 > 0) s(1,coset(0,0,lb),l) = s(1,coset(0,0,lb),l) +&
                             f2z*s(1,coset(0,0,lb-1),lz1)
                IF (ly1 > 0) s(1,coset(0,1,bz),l) = s(1,coset(0,1,bz),l) +&
                             f2y*s(1,coset(0,0,bz),ly1)
              DO by=2,lb
                s(1,coset(0,by,bz),l) = rbp(2)*s(1,coset(0,by-1,bz),l) +&
                                       f2*REAL(by-1,dp)*s(1,coset(0,by-2,bz),l)
                IF (lx1 > 0) s(1,coset(1,by,bz),l) = s(1,coset(1,by,bz),l) +&
                             f2x*s(1,coset(0,by,bz),lx1)
              END DO
              DO bx=2,lb
                DO by=0,lb-bx
                  IF (lx1 > 0) s(1,coset(bx,by,bz),l) = s(1,coset(bx,by,bz),l) +&
                               f2x*s(1,coset(bx-1,by,bz),lx1)
                END DO
              END DO
            END DO
          END IF
        END IF
        END DO
        DO k=2,ncoset(lc_max)
          DO j=1,ncoset(lb_max)
          END DO
        END DO
      END DO
    END DO
  END SUBROUTINE moment
  SUBROUTINE diff_momop(la_max,npgfa,zeta,rpgfa,la_min,&
                    order,rac,rbc,difmab,mab_ext)
    REAL(KIND=dp), DIMENSION(:, :, :), &
      OPTIONAL, POINTER                      :: mab_ext
    REAL(KIND=dp), ALLOCATABLE, &
      DIMENSION(:, :, :)                     :: difmab_tmp
    DO imom = 1,ncoset(order)-1
      CALL adbdr(la_max,npgfa,rpgfa,la_min,&
                 difmab_tmp(:,:,2), difmab_tmp(:,:,3))
    END DO
  END SUBROUTINE diff_momop
END MODULE ai_moments
! { dg-final { cleanup-modules "ai_moments" } }
