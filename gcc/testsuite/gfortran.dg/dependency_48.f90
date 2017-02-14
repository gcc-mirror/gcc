! { dg-do compile }
! { dg-options "-frepack-arrays -Warray-temporaries -O" }

! Same as dependency_35 but with repack-arrays

module foo
  implicit none
contains
  pure function bar(i,j) ! { dg-warning "Creating array temporary at \\(1\\)" }
    integer, intent(in) :: i,j
    integer, dimension(2,2) :: bar
    bar = 33
  end function bar
end module foo

program main
  use foo
  implicit none
  integer a(2,2), b(2,2),c(2,2), d(2,2), e(2)

  read (*,*) b, c, d
  a = matmul(b,c) + d
  a = b + bar(3,4)
  a = bar(3,4)*5 + b
  e = sum(b,1) + 3
end program main
