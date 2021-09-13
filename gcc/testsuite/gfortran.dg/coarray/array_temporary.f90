! { dg-do compile }
! { dg-additional-options "-Warray-temporaries" }
!
! PR fortran/98913
!
! Contributed by Jorge D'Elia
!
! Did create an array temporary for local access to coarray
! (but not for identical noncoarray use).
!

program test
  implicit none
  integer, parameter :: iin = kind (1)     
  integer, parameter :: idp = kind (1.0d0) 
  real    (kind=idp), allocatable :: AA (:,:)[:]
  real    (kind=idp), allocatable :: BB (:,:)
  real    (kind=idp), allocatable :: UU (:)
  integer (kind=iin) :: nn, n1, n2
  integer (kind=iin) :: j, k, k1
  !
  nn =  5
  n1 =  1
  n2 = 10
  !
  allocate (AA (1:nn,n1:n2)[*])
  allocate (BB (1:nn,n1:n2))
  allocate (UU (1:nn))
  !
  k  = 1
  k1 = k + 1
  !
  AA = 1.0_idp
  BB = 1.0_idp
  UU = 2.0_idp

  ! AA - coarrays
  ! No temporary needed:
  do  j = 1, nn
    AA (k1:nn,j) = AA (k1:nn,j) - UU (k1:nn) * AA (k,j)  ! { dg-bogus "Creating array temporary" }
  end do
  do  j = 1, nn
    AA (k1:nn,j) = AA (k1:nn,j) - UU (k1:nn) * AA (k,j) - UU(k) * AA (k1-1:nn-1,j)  ! { dg-bogus "Creating array temporary" }
  end do
  do  j = 1, nn
    AA (k1:nn,j) = AA (k1:nn,j) - UU (k1:nn) * AA (k,j) - UU(k) * AA (k1+1:nn+1,j)  ! { dg-bogus "Creating array temporary" }
  end do

  ! But:
  do  j = 1, nn
    AA (k1:nn,j) = AA (k1-1:nn-1,j) - UU (k1:nn) * AA (k,j) - UU(k) * AA (k1+1:nn+1,j)  ! { dg-warning "Creating array temporary" }
  end do

  ! BB - no coarrays
  ! No temporary needed:
  do  j = 1, nn
    BB (k1:nn,j) = BB (k1:nn,j) - UU (k1:nn) * BB (k,j)  ! { dg-bogus "Creating array temporary" }
  end do
  do  j = 1, nn
    BB (k1:nn,j) = BB (k1:nn,j) - UU (k1:nn) * BB (k,j) - UU(k) * BB (k1-1:nn-1,j)  ! { dg-bogus "Creating array temporary" }
  end do
  do  j = 1, nn
    BB (k1:nn,j) = BB (k1:nn,j) - UU (k1:nn) * BB (k,j) - UU(k) * BB (k1+1:nn+1,j)  ! { dg-bogus "Creating array temporary" }
  end do

  ! But:
  do  j = 1, nn
    BB (k1:nn,j) = BB (k1-1:nn-1,j) - UU (k1:nn) * BB (k,j) - UU(k) * BB (k1+1:nn+1,j)  ! { dg-warning "Creating array temporary" }
  end do

  deallocate (AA)
  deallocate (BB)
  deallocate (UU)
end program test
