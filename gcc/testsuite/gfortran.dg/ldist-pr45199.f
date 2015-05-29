! { dg-do compile }
! { dg-options "-O3 -fdump-tree-ldist-details" }

      parameter(numlev=3,numoblev=1000)
      integer i_otyp(numoblev,numlev), i_styp(numoblev,numlev)
      logical l_numob(numoblev,numlev)
      do ixe=1,numoblev
         do iye=1,numlev
            i_otyp(ixe,iye)=0
            i_styp(ixe,iye)=0
            l_numob(ixe,iye)=.false.
         enddo
      enddo
      do i=1,m
         do j=1,n
            if (l_numob(i,j)) then
               write(20,'(7I4,F12.2,4F16.10)') i_otyp(i,j),i_styp(i,j)
            endif
         enddo
      enddo
      end

! GCC should apply memset zero loop distribution and it should not ICE.

! { dg-final { scan-tree-dump "distributed: split to 0 loops and 9 library calls" "ldist" } }
! { dg-final { scan-tree-dump-times "generated memset zero" 9 "ldist" } }
