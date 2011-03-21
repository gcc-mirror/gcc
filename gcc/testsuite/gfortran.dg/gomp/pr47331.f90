! PR fortran/47331
! { dg-do compile }
! { dg-options "-fopenmp -fwhole-file" }

subroutine foo
  !$omp parallel
    call bar ()
  !$omp end parallel
end subroutine foo

subroutine bar
  integer :: k
  do k=1,5
    call baz (k)
  end do
end subroutine bar

subroutine baz (k)
  integer :: k
end subroutine

program pr47331
  call foo
end program pr47331
