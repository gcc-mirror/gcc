! { dg-options "-O2 -floop-interchange -fdump-tree-graphite-details --param graphite-allow-codegen-errors=1" }

      subroutine linel(icmdl,stre,anisox)
      real*8 stre(6),tkl(3,3),ekl(3,3),anisox(3,3,3,3)
            do m1=1,3
               do m2=1,m1
                  do m3=1,3
                     do m4=1,3
                        tkl(m1,m2)=tkl(m1,m2)+
     &                       anisox(m1,m2,m3,m4)*ekl(m3,m4)
                     enddo
                  enddo
               enddo
            enddo
            stre(1)=tkl(1,1)
      end

! { dg-final { scan-tree-dump-times "code generation error" 1 "graphite" } }
