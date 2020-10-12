! { dg-do run }

! PR/fortran 93671 - ICE on intrinsic assignment to allocatable derived-type
!                    component of coarray

  type flux_planes
    integer, allocatable :: normals
  end type

  type package
    type(flux_planes) surface_fluxes(1)
  end type

  type(package) mail[*], halo_data

  halo_data%surface_fluxes(1)%normals = 1
  mail = halo_data
  
  if (any(size(mail%surface_fluxes) /= [1]) .OR. &
          mail%surface_fluxes(1)%normals /= 1) then
    stop 1
  end if
end

