! { dg-do compile }
! { dg-additional-options "-ffast-math -fdump-tree-slp1" }

      subroutine shell(nx,ny,nz,q,dt,cfl,dx,dy,dz,cfll,gm,Pr,Re)
      implicit none
      integer nx,ny,nz,i,j,k
      real*8 cfl,dx,dy,dz,dt
      real*8 gm,Re,Pr,cfll,t1,t2,t3,t4,t5,t6,t7,t8,mu
      real*8 q(5,nx,ny,nz)

      if (cfll.ge.cfl) cfll=cfl
      t8=0.0d0

      do k=1,nz
         do j=1,ny
            do i=1,nx
               t1=q(1,i,j,k)
               t2=q(2,i,j,k)/t1
               t3=q(3,i,j,k)/t1
               t4=q(4,i,j,k)/t1
               t5=(gm-1.0d0)*(q(5,i,j,k)-0.5d0*t1*(t2*t2+t3*t3+t4*t4))
               t6=dSQRT(gm*t5/t1)
               mu=gm*Pr*(gm*t5/t1)**0.75d0*2.0d0/Re/t1
               t7=((dabs(t2)+t6)/dx+mu/dx**2)**2 +
     1            ((dabs(t3)+t6)/dy+mu/dy**2)**2 +
     2            ((dabs(t4)+t6)/dz+mu/dz**2)**2
               t7=DSQRT(t7)
               t8=max(t8,t7)
            enddo
         enddo
      enddo
      dt=cfll / t8

      return
      end

! We don't have an effective target for reduc_plus_scal optab support
! { dg-final { scan-tree-dump ".REDUC_PLUS" "slp1" { target x86_64-*-* } } }
