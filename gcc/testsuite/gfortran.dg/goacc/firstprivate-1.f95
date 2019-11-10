! { dg-do compile }

program test
  integer a, b(100)

  !$acc parallel firstprivate (a, b)
  !$acc end parallel

  !$acc parallel firstprivate (b(10:20)) ! { dg-error "Syntax error in OpenMP variable list" }
  !$acc end parallel ! { dg-error "Unexpected !\\\$ACC END PARALLEL statement" }


  !$acc serial firstprivate (a, b)
  !$acc end serial

  !$acc serial firstprivate (b(10:20)) ! { dg-error "Syntax error in OpenMP variable list" }
  !$acc end serial ! { dg-error "Unexpected !\\\$ACC END SERIAL statement" }

end program test
