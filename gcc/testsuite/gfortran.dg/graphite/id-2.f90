module solv_cap
  integer, parameter, public :: dp = selected_real_kind(5)
contains
  subroutine prod0( G, X )
    real(kind=dp), intent(in out), dimension(:,:)    :: X
    real(kind=dp), dimension(size(X,1),size(X,2)) :: Y
    X = Y
  end subroutine prod0
  function Ginteg(xq1,yq1, xq2,yq2, xp,yp)  result(G)
  end function Ginteg
  subroutine fourir(A,ntot,kconjg, E,useold)
  end subroutine fourir
end module solv_cap
! { dg-final { cleanup-modules "solv_cap" } }
