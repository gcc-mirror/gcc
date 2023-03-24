subroutine test(i)
  ! TODO The checking that produces this message comes too late. Not important, but would be nice to have.
  !$omp unroll full ! { dg-error {missing canonical loop nest after \!\$OMP UNROLL at \(1\)} "" { xfail *-*-* } }
  call dummy0 ! { dg-error {Unexpected CALL statement at \(1\)} }
end subroutine test ! { dg-error {Unexpected END statement at \(1\)} }

! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
