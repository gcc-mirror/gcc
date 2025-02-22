! { dg-do run }
! { dg-options "-fdump-tree-original -fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

program Jac
  type Domain
    integer :: n=64
    integer,allocatable :: endsi(:)
  end type
  type(Domain),allocatable :: D[:,:,:]

  allocate(D[2,2,*])
  allocate(D%endsi(2), source = 0)
  ! Lhs may be reallocate. Due to new communication pattern no send.
  D%endsi = D%n
  if (any(D%endsi /= [ 64, 64])) error stop
  deallocate(D)
end program

! { dg-final { scan-tree-dump-not "caf_send" "original" } }

