! { dg-do compile }
! { dg-additional-options "-Ofast -mlsx -fvect-cost-model=dynamic" { target loongarch64*-*-* } }

MODULE material_mod

IMPLICIT NONE

integer, parameter :: dfp = selected_real_kind (13, 99)
integer, parameter :: rfp = dfp

PUBLIC Mat_updateE, iepx, iepy, iepz

PRIVATE

integer, dimension (:, :, :), allocatable :: iepx, iepy, iepz
real (kind = rfp), dimension (:), allocatable :: Dbdx, Dbdy, Dbdz
integer :: imin, jmin, kmin
integer, dimension (6) :: Exsize
integer, dimension (6) :: Eysize
integer, dimension (6) :: Ezsize
integer, dimension (6) :: Hxsize
integer, dimension (6) :: Hysize
integer, dimension (6) :: Hzsize

CONTAINS

SUBROUTINE mat_updateE (nx, ny, nz, Hx, Hy, Hz, Ex, Ey, Ez)

integer, intent (in) :: nx, ny, nz

real (kind = rfp), intent (inout),                                         &
  dimension (Exsize (1) : Exsize (2), Exsize (3) : Exsize (4), Exsize (5) : Exsize (6)) :: Ex
real (kind = rfp), intent (inout),                                         &
  dimension (Eysize (1) : Eysize (2), Eysize (3) : Eysize (4), Eysize (5) : Eysize (6)) :: Ey
real (kind = rfp), intent (inout),                                         &
  dimension (Ezsize (1) : Ezsize (2), Ezsize (3) : Ezsize (4), Ezsize (5) : Ezsize (6)) :: Ez
real (kind = rfp), intent (in),                                            &
  dimension (Hxsize (1) : Hxsize (2), Hxsize (3) : Hxsize (4), Hxsize (5) : Hxsize (6)) :: Hx
real (kind = rfp), intent (in),                                            &
  dimension (Hysize (1) : Hysize (2), Hysize (3) : Hysize (4), Hysize (5) : Hysize (6)) :: Hy
real (kind = rfp), intent (in),                                            &
  dimension (Hzsize (1) : Hzsize (2), Hzsize (3) : Hzsize (4), Hzsize (5) : Hzsize (6)) :: Hz

integer :: i, j, k, mp

do k = kmin, nz
  do j = jmin, ny
    do i = imin, nx
      mp = iepx (i, j, k)
      Ex (i, j, k) = Ex (i, j, k) +                                        &
                  Dbdy (mp) * (Hz (i, j, k  ) - Hz (i, j-1, k)) +          &
                  Dbdz (mp) * (Hy (i, j, k-1) - Hy (i, j  , k))

      mp = iepy (i, j, k)
      Ey (i, j, k) = Ey (i, j, k) +                                        &
                  Dbdz (mp) * (Hx (i  , j, k) - Hx (i, j, k-1)) +          &
                  Dbdx (mp) * (Hz (i-1, j, k) - Hz (i, j, k  ))

      mp = iepz (i, j, k)
      Ez (i, j, k) = Ez (i, j, k) +                                        &
                  Dbdx (mp) * (Hy (i, j  , k) - Hy (i-1, j, k)) +          &
                  Dbdy (mp) * (Hx (i, j-1, k) - Hx (i  , j, k))
    end do
  end do
end do

END SUBROUTINE mat_updateE

END MODULE material_mod

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target loongarch64*-*-* } } }
