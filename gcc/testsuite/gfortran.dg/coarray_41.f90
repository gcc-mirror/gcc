! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }
! { dg-additional-options "-latomic" { target libatomic_available } }

program coarray_41

  integer, allocatable :: vec(:)[:,:]

  allocate(vec(10)[2,*], source= 37)

  if (.not. allocated(vec)) error stop

  call foo(vec)

  if (any(vec /= 42)) error stop

  deallocate(vec)
contains

  subroutine foo(gv)

    integer, allocatable, intent(inout) :: gv(:)[:,:]
    integer, allocatable :: gvin(:)

    allocate(gvin, mold=gv)
    gvin = 5
    gv = gv + gvin
  end subroutine foo

end program coarray_41
