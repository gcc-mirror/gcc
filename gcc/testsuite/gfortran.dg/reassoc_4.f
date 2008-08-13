! { dg-do compile }
! { dg-options "-O3 -ffast-math -fdump-tree-reassoc1" }
      subroutine anisonl(w,vo,anisox,s,ii1,jj1,weight)
      integer ii1,jj1,i1,iii1,j1,jjj1,k1,l1,m1,n1
      real*8 w(3,3),vo(3,3),anisox(3,3,3,3),s(60,60),weight
!
!     This routine replaces the following lines in e_c3d.f for
!     an anisotropic material
!
                      do i1=1,3
                        iii1=ii1+i1-1
                        do j1=1,3
                          jjj1=jj1+j1-1
                          do k1=1,3
                            do l1=1,3
                              s(iii1,jjj1)=s(iii1,jjj1)
     &                          +anisox(i1,k1,j1,l1)*w(k1,l1)*weight
                              do m1=1,3
                                s(iii1,jjj1)=s(iii1,jjj1)
     &                              +anisox(i1,k1,m1,l1)*w(k1,l1)
     &                                 *vo(j1,m1)*weight
     &                              +anisox(m1,k1,j1,l1)*w(k1,l1)
     &                                 *vo(i1,m1)*weight
                                do n1=1,3
                                  s(iii1,jjj1)=s(iii1,jjj1)
     &                              +anisox(m1,k1,n1,l1)
     &                              *w(k1,l1)*vo(i1,m1)*vo(j1,n1)*weight
                                enddo
                              enddo
                            enddo
                          enddo
                        enddo
                      enddo

      return
      end

! There should be 22 multiplications left after un-distributing
! weigth, w(k1,l1), vo(i1,m1) and vo(j1,m1) on the innermost two
! unrolled loops.

! { dg-final { scan-tree-dump-times "\[0-9\] \\\* " 22 "reassoc1" } }
! { dg-final { cleanup-tree-dump "reassoc1" } }
