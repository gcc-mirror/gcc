! { dg-do run }

! PR fortran/45384
! Double free happened, check that it works now.

! Contributed by Salvatore Filippone  <salvatore.filippone@uniroma2.it>

program bug20

  type :: d_base_sparse_mat
    integer :: v(10) = 0.
  end type d_base_sparse_mat

  class(d_base_sparse_mat),allocatable :: a

  allocate (d_base_sparse_mat :: a)

  select type(aa => a)
  type is (d_base_sparse_mat)
    write(0,*) 'NV = ',size(aa%v)
    if (size(aa%v) /= 10) STOP 1
  class default 
    write(0,*) 'Not implemented yet '
  end select

end program bug20
