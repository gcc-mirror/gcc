! { dg-do compile }
program do_concurrent_parser_errors
  implicit none
  integer :: i, x, b
  do, concurrent (i=-3:4:2) default(none) shared(b) default(none)  ! { dg-error "DEFAULT \\(NONE\\) specified more than once in DO CONCURRENT" }
    b = i
  end do ! { dg-error "Expecting END PROGRAM statement" }
  do concurrent(i = 2 : 4) reduce(-:x)  ! { dg-error "Expected reduction operator or function name" }
    x = x - i
  end do ! { dg-error "Expecting END PROGRAM statement" }
  do concurrent(i = 2 : 4) reduce(+ x)  ! { dg-error "Expected ':'" }
    x = x + i
  end do ! { dg-error "Expecting END PROGRAM statement" }
  do concurrent(i = 2 : 4) reduce(+ , x)  ! { dg-error "Expected ':'" }
    x = x + i
  end do ! { dg-error "Expecting END PROGRAM statement" }
  do concurrent(i = 2 : 4) reduction(+: x)  ! { dg-error "Syntax error in DO statement" }
    x = x + i
  end do ! { dg-error "Expecting END PROGRAM statement" }
end program do_concurrent_parser_errors
