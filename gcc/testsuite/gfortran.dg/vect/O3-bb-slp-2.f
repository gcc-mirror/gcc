! { dg-do compile }
! { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } }
       subroutine tranx3 (ibeg,jbeg,jend,kbeg,kend
     &                   ,dlo,den
     &                   ,edn)
      parameter(in =           128+5
     &        , jn =           128+5
     &        , kn =           128+5)
      parameter(ijkn =   128+5)
      real*8   e (in,jn,kn), dqm, dvl3a  (kn), dvl3ai (kn)
     &           , dtwid (ijkn,4),  dd    (ijkn,4)
     &           , etwid (ijkn,4),  deod  (ijkn,4)
       real*8  dlo  (in,jn,kn), den  (in,jn,kn)
     &      , edn  (in,jn,kn)
       do 2100 j=jbeg-1,jend
           i = ibeg - 1
               do 1080 k=kbeg,kend
               den(i  ,j,k) = ( dlo(i  ,j,k) * dvl3a(k)
     1                      - etwid (k+1,1) + etwid (k,1) ) * dvl3a i(k)
1080           continue
             do 2030 k=max(kbeg-2,ks-1),kend+1
               dqm      = (dlo(i+2,j,k  ) - dlo(i+2,j,k-1)) * dx3bi(k  )
               dd(k,4)  = max ( dqm * dqp, zro )
2030       continue
               dtwid (k,3) = ( 0.5 + q1 ) * ( dlo(i+2,j,k-1)
     1                     + ( dx3a(k-1) - xi ) * dd   (k-1,3) )
     2                     + ( 0.5 - q1 ) * ( dlo(i+2,j,k  )
     3                     - ( dx3a(k  ) + xi ) * deod (k  ,3) )
               do 2080 k=kbeg,kend
               den(i  ,j,k) = ( dlo(i  ,j,k) * dvl3a(k)
     1                      - dtwid (k+1,3) + dtwid (k,3) ) * dvl3a i(k)
               e  (i+2,j,k) = ( e  (i+2,j,k) * dvl3a(k)
     1                      - etwid (k+1,3) + etwid (k,3) ) * dvl3a i(k)
               edn(i+2,j,k) =         e(i+2,j,k) / den(i+2,j,k)
               e  (i+3,j,k) = ( e  (i+3,j,k) * dvl3a(k)
     1                      - etwid (k+1,4) + etwid (k,4) ) * dvl3a i(k)
               edn(i+3,j,k) =         e(i+3,j,k) / den(i+3,j,k)
2080           continue
2100   continue
       end
