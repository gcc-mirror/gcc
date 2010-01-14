! { dg-options "-O2 -fgraphite-identity" }

      parameter(in =           128+5
     &        , jn =           128+5
     &        , kn =           128+5)
      real*8   d (in,jn,kn)
       real*8 dcopy(in,jn,kn)
       call pdv (is, dcopy)
       do k=ks,ke
         do j=je+1,je+2
           do i=is-2,ie+2
             dcopy(i,j,k) = d(i,j,k)
           enddo
         enddo
       enddo
       do k=ks,ke
         do j=js,je
           do i=is-2,is-1
             dcopy(i,j,k) = d(i,j,k)
           enddo
         enddo
       enddo
       end
