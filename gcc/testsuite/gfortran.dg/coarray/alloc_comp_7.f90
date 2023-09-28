! { dg-do run }

module alloc_comp_module_7

  public :: check

  type :: foo
    real :: x
    integer, allocatable :: y(:)
  contains
    final :: foo_final
  end type

contains

  subroutine foo_final(f)
    type(foo), intent(inout) :: f

    if (allocated(f%y)) then
      f%y = -1
    end if
  end subroutine foo_final

  subroutine check()
    block
      type(foo), allocatable :: example[:] ! needs to be a coarray

      allocate(example[*])
      allocate(example%y(10))
      example%x = 3.4
      example%y = 4

      deallocate(example%y)
      deallocate(example)
    end block  ! example%y shall not be accessed here by the finalizer,
               ! because example is already deallocated
  end subroutine check
end module alloc_comp_module_7

program alloc_comp_7

  use alloc_comp_module_7, only: check

  implicit none

  call check()

end program alloc_comp_7

