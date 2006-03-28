! PR fortran/16861
! { dg-do run }
module foo
  integer :: i
end module foo

module bar
contains
  subroutine baz(j)
    use foo
    integer, dimension(i) :: j
    integer :: n

    do n = 1, i
      if (j(n) /= n**2) call abort
    end do
  end subroutine baz
end module bar

subroutine quus()
  use foo
  use bar

  i = 2
  call baz ((/1,4/))
  i = 7
  call baz ((/1,4,9,16,25,36,49/))
end subroutine quus

program test
  call quus
end program test

! { dg-final { cleanup-modules "foo bar" } }
