! { dg-do compile }
! { dg-require-effective-target pthread }
! { dg-options "-Ofast -ftree-parallelize-loops=4" }

SUBROUTINE wsm32D(t, &
   w, &
   den, &
   p, &
   delz, &
                     its,&
   ite, &
   kts, &
   kte  &
                      )
  REAL, DIMENSION( its:ite , kts:kte ),                           &
        INTENT(INOUT) ::                                          &
                                                               t
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(IN   ) ::                                       w, &
                                                             den, &
                                                               p, &
                                                            delz
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
        qs, &
        xl, &
        work1, &
        work2, &
        qs0, &
        n0sfac
      diffus(x,y) = 8.794e-5*x**1.81/y
      diffac(a,b,c,d,e) = d*a*a/(xka(c,d)*rv*c*c)+1./(e*diffus(c,b))
      venfac(a,b,c) = (viscos(b,c)/diffus(b,a))**(.3333333)       &
             /viscos(b,c)**(.5)*(den0/c)**0.25
      do loop = 1,loops
      xa=-dldt/rv
      do k = kts, kte
        do i = its, ite
          tr=ttp/t(i,k)
          if(t(i,k).lt.ttp) then
            qs(i,k) =psat*(tr**xa)*exp(xb*(1.-tr))
          endif
          qs0(i,k)  =psat*(tr**xa)*exp(xb*(1.-tr))
        enddo
        do i = its, ite
          if(t(i,k).ge.t0c) then
            work1(i,k) = diffac(xl(i,k),p(i,k),t(i,k),den(i,k),qs(i,k))
          endif
          work2(i,k) = venfac(p(i,k),t(i,k),den(i,k))
        enddo
      enddo
      enddo                  ! big loops
END SUBROUTINE wsm32D
