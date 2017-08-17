! { dg-do compile }
! { dg-options "-std=gnu -fno-tree-scev-cprop -Ofast" }

subroutine CalcCgr(C,rmax,ordgr_max)
  integer, intent(in) :: rmax,ordgr_max
  double complex :: Zadj(2,2), Zadj2(2,2)
  double complex, intent(out) :: C(0:rmax,0:rmax,0:rmax)
  double complex, allocatable :: Cexpgr(:,:,:,:)
  double complex :: Caux
  integer :: rmaxB,rmaxExp,r,n0,n1,n2,k,l,i,j,m,n,nn

  rmaxB = 2*rmax
  rmaxExp = rmaxB
  allocate(Cexpgr(0:rmaxExp/2,0:rmaxExp,0:rmaxExp,0:ordgr_max))
   
  rloop: do r=0,rmaxExp/2
    do n0=r,1,-1
      do nn=r-n0,0,-1
        do i=1,2
          Caux = Caux - Zadj(i,l)
        end do
        Cexpgr(n0,0,0,0) = Caux/(2*(nn+1))
      end do
    end do
    do n1=0,r
      n2 = r-n1
      if (r.le.rmax) then
        C(0,n1,n2) = Cexpgr(0,n1,n2,0)
      end if
    end do
  end do rloop
end subroutine CalcCgr
