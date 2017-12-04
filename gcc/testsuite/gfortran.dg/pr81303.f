! { dg-do compile }
! { dg-options "-O3 -ffast-math -floop-interchange -fdump-tree-linterchange-details" }

        subroutine mat_times_vec(y,x,a,axp,ayp,azp,axm,aym,azm,
     $  nb,nx,ny,nz)
        implicit none
        integer nb,nx,ny,nz,i,j,k,m,l,kit,im1,ip1,jm1,jp1,km1,kp1

        real*8 y(nb,nx,ny,nz),x(nb,nx,ny,nz)

        real*8 a(nb,nb,nx,ny,nz),
     1  axp(nb,nb,nx,ny,nz),ayp(nb,nb,nx,ny,nz),azp(nb,nb,nx,ny,nz),
     2  axm(nb,nb,nx,ny,nz),aym(nb,nb,nx,ny,nz),azm(nb,nb,nx,ny,nz)


      do k=1,nz
         km1=mod(k+nz-2,nz)+1
         kp1=mod(k,nz)+1
         do j=1,ny
            jm1=mod(j+ny-2,ny)+1
            jp1=mod(j,ny)+1
            do i=1,nx
               im1=mod(i+nx-2,nx)+1
               ip1=mod(i,nx)+1
               do l=1,nb
                  y(l,i,j,k)=0.0d0
                  do m=1,nb
                     y(l,i,j,k)=y(l,i,j,k)+
     1               a(l,m,i,j,k)*x(m,i,j,k)+
     2               axp(l,m,i,j,k)*x(m,ip1,j,k)+
     3               ayp(l,m,i,j,k)*x(m,i,jp1,k)+
     4               azp(l,m,i,j,k)*x(m,i,j,kp1)+
     5               axm(l,m,i,j,k)*x(m,im1,j,k)+
     6               aym(l,m,i,j,k)*x(m,i,jm1,k)+
     7               azm(l,m,i,j,k)*x(m,i,j,km1)
                  enddo
               enddo
            enddo
         enddo
        enddo          
        return
        end

! { dg-final { scan-tree-dump-times "is interchanged" 1 "linterchange" } }
