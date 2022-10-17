! { dg-do compile }
! PR fortran/102956
! PDT KIND and LEN type parameters are mutually exclusive (F2018:R734)
!
module m
  type :: good_pdt (k,l)
     integer, kind           :: k = 1
     integer, len            :: l = 1
     character(kind=k,len=l) :: c
  end type good_pdt

  type :: bad_pdt (k,l)               ! { dg-error "does not have a component" }
     integer, kind, len      :: k = 1 ! { dg-error "attribute conflicts with" }
     integer, len, kind      :: l = 1 ! { dg-error "attribute conflicts with" }
     character(kind=k,len=l) :: c     ! { dg-error "has not been declared" }
  end type bad_pdt
end
