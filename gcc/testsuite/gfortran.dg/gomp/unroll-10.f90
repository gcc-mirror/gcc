subroutine test(i)
  !$omp unroll full
  call dummy0 ! { dg-error "Unexpected CALL statement at \\\(1\\\)" }
end subroutine test ! { dg-error "Unexpected END statement at \\\(1\\\)" }

! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
