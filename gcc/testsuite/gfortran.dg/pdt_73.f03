! { dg-do compile }
!
! Tests the fix for pr122669, which falied with the error below.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
  implicit none
  type tensor_t
    real, allocatable :: values_
  end type
  type(tensor_t) :: random_inputs(1)
  type(tensor_t), allocatable :: outputs(:)

  random_inputs = [tensor_t(1.0)]
  allocate(outputs, mold=random_inputs) ! Error: Array specification or array-valued
                                        ! SOURCE= expression required in ALLOCATE statement at (1)
  print *, size(outputs)
end 
