! { dg-do compile }
program do_concurrent_all_clauses
  implicit none
  integer :: i, arr(10), sum, max_val, temp, squared
  sum = 0
  max_val = 0

  do concurrent (i = 1:10, i <= 8) &
      default(none) &
      local(temp) &
      shared(arr, squared, sum, max_val) &
      reduce(+:sum) & ! { dg-error "Variable 'sum' at \\(1\\) has already been specified in a locality-spec" }
      reduce(max:max_val) ! { dg-error "Variable 'max_val' at \\(1\\) has already been specified in a locality-spec" }
    block
      integer :: temp2
      temp = i * 2
      temp2 = temp * 2
      squared = i * i
      arr(i) = temp2 + squared
      sum = sum + arr(i)
      max_val = max(max_val, arr(i)) ! { dg-error "Reference to impure function" }
    end block
  end do
  print *, arr, sum, max_val
end program do_concurrent_all_clauses
