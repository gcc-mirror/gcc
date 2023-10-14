subroutine f()
  !$omp parallel
  block
  end block

  !$omp parallel
  block
    inner: block
       block
       end block
    end block inner
  end block
end

subroutine f2()
  !$omp parallel
  my_name : block
  end block my_name

  !$omp parallel
  another_block : block
    inner: block
       block
       end block
    end block inner
  end block another_block
end

subroutine f3()
  !$omp parallel
  my_name : block
  end block my_name2  ! { dg-error "Expected label 'my_name' for END BLOCK statement" }
  end block my_name   ! avoid follow up errors
end subroutine

subroutine f4
  integer :: n
  n = 5
  !$omp parallel
  my: block
    integer :: A(n)
    A(1) = 1
  end block my
end

subroutine f4a
  intrinsic :: sin
  !$omp parallel
  block
    procedure(), pointer :: proc
    procedure(sin) :: my_sin
    proc => sin
  end block
end subroutine

subroutine f5(x)
  !$omp parallel
  block
    intent(in) :: x  ! { dg-error "INTENT is not allowed inside of BLOCK" }
    optional :: x    ! { dg-error "OPTIONAL is not allowed inside of BLOCK" }
    value :: x       ! { dg-error "VALUE is not allowed inside of BLOCK" }
  end block
end

subroutine f6()
  !$omp parallel
  myblock: block
    cycle myblock !  { dg-error "CYCLE statement at .1. is not applicable to non-loop construct 'myblock'" }
  end block myblock

  !$omp parallel
  myblock2: block
    exit  myblock2 ! OK.
    ! jumps to the end of the block but stays in the structured block
  end block myblock2
  !$omp end parallel
end
