! { dg-do compile }

  double precision :: arr(5, 8)
  call bar (arr)
contains
  subroutine foo (arr)
    double precision :: arr(:,:)
    arr(3, 4) = 24
  end subroutine foo
  subroutine bar (arr)
    double precision :: arr(5,*)
    call foo (arr)	! { dg-error "cannot be an assumed-size array" }
    call foo (arr (:, :8))
  end subroutine
end
