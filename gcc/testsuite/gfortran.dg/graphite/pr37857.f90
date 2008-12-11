! { dg-options "-O2 -floop-block" }

program superficie_proteina
  integer, parameter :: LONGreal = selected_real_kind(12,90)
  integer :: number_of_polypeptides, maximum_polypeptide_length
  real (kind = LONGreal), dimension (:,:), allocatable :: individual_conformations
  allocate (individual_conformations(-number_of_bins:0,number_of_polypeptides))
  individual_conformations = 0.0_LONGreal
end program superficie_proteina
