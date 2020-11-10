! { dg-do compile }
       subroutine tranx3 (jbeg,jend,kbeg,kend,dlo,den,mflx,zro)
      parameter(in =           128+5
     &        , jn =           128+5
     &        , kn =           128+5)
      parameter(ijkn =   128+5)
      real*8    zro, dqm, dqp, dx3bi  (kn)
      real*8    mflux (ijkn,4), dtwid (ijkn,4),  dd    (ijkn,4)
       real*8  mflx (in,jn,kn)
       real*8  dlo  (in,jn,kn), den  (in,jn,kn)
       do 2100 j=jbeg-1,jend
               dtwid (k,1) = ( 0.5 + q1 ) * ( dlo(i  ,j,k-1)
     3                     - ( dx3a(k  ) + xi ) * dd   (k  ,1) )
               mflux (k,1) = dtwid (k,1) * ( v3(i  ,j,k) - vg3(k) ) * dt
             if (j.ge.jbeg) then
               den(i  ,j,k) = ( dlo(i  ,j,k) * dvl3a(k)
     1                      - etwid (k+1,1) + etwid (k,1) ) * dvl3a i(k)
               if (kend .eq. ke) mflx(i  ,j,ke+1) = mflux (ke+1,1)
             endif
             do 2030 k=max(kbeg-2,ks-1),kend+1
               dqm      = (dlo(i  ,j,k  ) - dlo(i  ,j,k-1)) * dx3bi(k  )
               dqp      = (dlo(i  ,j,k+1) - dlo(i  ,j,k  )) * dx3bi(k+1)
               dd(k,1)  = max ( dqm * dqp, zro )
2030       continue
               dtwid (k,3) = ( 0.5 + q1 ) * ( dlo(i+2,j,k-1)
     3                     - ( dx3a(k  ) + xi ) * deod (k  ,3) )
2100   continue
       end
