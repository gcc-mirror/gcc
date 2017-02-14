! { dg-do run }

program Jac
  type Domain
    integer :: n=64
    integer,allocatable :: endsi(:)
  end type
  type(Domain),allocatable :: D[:,:,:]

  allocate(D[2,2,*])
  allocate(D%endsi(2), source = 0)
  ! No caf-runtime call needed her.
  D%endsi(2) = D%n
  if (any(D%endsi /= [ 0, 64])) error stop
  deallocate(D)
end program

