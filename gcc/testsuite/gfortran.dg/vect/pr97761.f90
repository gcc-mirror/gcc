! { dg-do compile }
! { dg-additional-options "-O1" }

subroutine ni (ps, inout)
    type vector
       real  x, y
    end type 
    type quad_inductor
       type (vector) v1, v2
    end type 
    type (quad_inductor), dimension(inout) :: ps
    integer :: dl, nk = 1.0
    fo = 1.0
    if (f == 1) then
       nk = 0.0
       fo = 0.0
    end if
    ot = nk * 0.5
    gb = -fo * 0.5
    wu = fo * 0.5
    up = nk * 0.1
    xe = up * 0.1
    do lx = 0, 7
       ps%v2%y = -wu
       ps(dl)%v1%x = xe + 1.0
       ps(dl)%v1%y = wu - tn
    end do
    do lx = 0, 7
       ps(dl)%v1%x = 0.1 - ot
       ps(dl)%v1%y = 0.1 - wu
    end do
end
