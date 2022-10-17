! { dg-do compile }
!
! Check that a requires directive is still recognized
! if it is in the associated parent namespace of the
! target directive.
!

module m
  !$omp requires reverse_offload
contains
  subroutine foo()
    !$omp target device(ancestor:1)
    !$omp end target
  end subroutine foo

  subroutine bar()
    block
      block
        block
          !$omp target device(ancestor:1)
          !$omp end target
        end block
      end block
    end block
  end subroutine bar
end module m

subroutine foo()
  !$omp requires reverse_offload
  block
    block
      block
        !$omp target device(ancestor:1)
        !$omp end target
      end block
    end block
  end block
contains
  subroutine bar()
    block
      block
        block
          !$omp target device(ancestor:1)
          !$omp end target
        end block
      end block
    end block
  end subroutine bar
end subroutine foo

program main
  !$omp requires reverse_offload
contains
  subroutine foo()
    !$omp target device(ancestor:1)
    !$omp end target
  end subroutine foo

  subroutine bar()
    block
      block
        block
          !$omp target device(ancestor:1)
          !$omp end target
        end block
      end block
    end block
  end subroutine bar
end
