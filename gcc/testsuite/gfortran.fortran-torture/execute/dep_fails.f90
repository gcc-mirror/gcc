! This gives incorrect results when compiled with
! the intel and pgf90 compilers
Program Strange

 Implicit None

 Type Link
 Integer, Dimension(2) :: Next
 End Type Link

 Integer, Parameter :: N = 2
 Integer, dimension (2, 4) :: results
 Integer :: i, j

 Type(Link), Dimension(:,:), Pointer :: Perm
 Integer, Dimension(2) :: Current

 Allocate (Perm(N,N))

! Print*, 'Spanned by indices'
 Do i = 1, N**2
    Perm(mod(i-1,N)+1, (i-1)/N+1)%Next = (/ Mod(i,N) + 1, Mod(i/N+1,N)+1/)
!    Write(*,100) mod(i-1,N)+1, (i-1)/N+1, Perm(mod(i-1,N)+1, (i-1)/N+1)%Next
! Expected output:
!  Spanned by indices
!  1 1---> 2 2
!  2 1---> 1 1
!  1 2---> 2 1
!  2 2---> 1 2
 End Do

! Print*, 'Spanned as a cycle'
 Current = (/1,1/) 
 Do i = 1, n**2
   results (:, i) = Perm(Current(1), Current(2))%Next
!    Write(*,100) Current, Perm(Current(1), Current(2))%Next 
! Expected output:
!  1 1---> 2 2
!  2 2---> 1 2
!  1 2---> 2 1
!  2 1---> 1 1
   Current = Perm(Current(1), Current(2))%Next
 End Do

 if (any(results .ne. reshape ((/2,2,1,2,2,1,1,1/), (/2, 4/)))) call abort

! 100 Format( 2I3, '--->', 2I3)
 DeAllocate (Perm)

End Program Strange
