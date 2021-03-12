! { dg-do compile }
! pr98686
  implicit none
  real    :: x, m
  namelist /NML/ x, m, q ! { dg-error "must be declared before the namelist*" }
  integer :: q
  x = 1.0
  m = 2.0
  q = 3
  write(*, nml=NML)
end
