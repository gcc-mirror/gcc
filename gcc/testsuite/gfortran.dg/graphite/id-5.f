      subroutine shell(Re,Pr,nx,ny,nz,
     $nuim,nuex2,nuex4,cfl,scheme,conf,ni,maxit) 
      real*8 q(5,nx,ny,nz),dq(5,nx,ny,nz),rhs(5,nx,ny,nz),e(5,nx,ny,nz),
     1     f(5,nx,ny,nz),g(5,nx,ny,nz),ev(5,nx,ny,nz),fv(5,nx,ny,nz),
     2     gv(5,nx,ny,nz),diss(5,nx,ny,nz)
      do k=1,nz
         do j=1,ny
            do i=1,nx
               do l=1,5
                  t1= -0.5d0*dt*(
     3            (g(l,i,j,kp1)-g(l,i,j,km1))/dz) +
     4            dt/Re*((ev(l,i,j,k)-ev(l,im1,j,k))/dx +
     6                  (gv(l,i,j,k)-gv(l,i,j,km1))/dz)
                  rhs(l,i,j,k)=t1+t2
               enddo
            enddo
         enddo
      enddo
      end
