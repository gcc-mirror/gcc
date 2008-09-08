! { dg-do run }
! { dg-options "-fbounds-check" }

! PR fortran/37199
! We used to produce wrong (segfaulting) code for this one because the
! temporary array for the function result had wrong bounds.

! Contributed by Gavin Salam <salam@lpthe.jussieu.fr>

program bounds_issue
  implicit none
  integer, parameter  :: dp = kind(1.0d0)
  real(dp), pointer :: pdf0(:,:), dpdf(:,:)

  allocate(pdf0(0:282,-6:7))
  allocate(dpdf(0:282,-6:7))  ! with dpdf(0:283,-6:7) [illegal] error disappears
  !write(0,*) lbound(dpdf), ubound(dpdf)
  dpdf = tmp_PConv(pdf0)

contains
  function tmp_PConv(q_in) result(Pxq)
    real(dp),      intent(in) :: q_in(0:,-6:)
    real(dp)                  :: Pxq(0:ubound(q_in,dim=1),-6:7)
    Pxq = 0d0
    !write(0,*) lbound(q_in), ubound(q_in)
    !write(0,*) lbound(Pxq),  ubound(Pxq)
    return
  end function tmp_PConv

end program bounds_issue
