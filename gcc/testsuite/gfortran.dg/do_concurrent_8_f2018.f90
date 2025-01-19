! { dg-do compile }
! { dg-options "-std=f2018" }
program do_concurrent_complex
  implicit none
  integer :: i, j, k, sum, product
  integer, dimension(10,10,10) :: array
  sum = 0
  product = 1
  do concurrent (i = 1:10) local(j) shared(sum) reduce(+:sum) ! { dg-error "Fortran 2023: REDUCE locality spec" }
    do concurrent (j = 1:10) local(k) shared(product) reduce(*:product) ! { dg-error "Fortran 2023: REDUCE locality spec" }
      do concurrent (k = 1:10)
        array(i,j,k) = i * j * k
        sum = sum + array(i,j,k)
        product = product * array(i,j,k)
      end do
    end do ! { dg-error "Expecting END PROGRAM statement" }
  end do ! { dg-error "Expecting END PROGRAM statement" }
  print *, sum, product
end program do_concurrent_complex