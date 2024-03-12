! { dg-do run }

program alloc_comp_6

  implicit none

  type :: foo
    real :: x
    integer, allocatable :: y(:)
  end type

  call check()

contains

  subroutine check()
    block
      type(foo), allocatable :: example[:] ! needs to be a coarray

      allocate(example[*])
      allocate(example%y(10))
      example%x = 3.4
      example%y = 4

      deallocate(example)
    end block  ! example%y shall not be accessed here by the finalizer,
               ! because example is already deallocated
  end subroutine check
end program alloc_comp_6
