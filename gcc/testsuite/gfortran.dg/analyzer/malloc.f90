! { dg-do compile }
! { dg-additional-options "-fcray-pointer -O0" }

subroutine test_ok
  real*8 x(*)
  pointer(ptr_x,x)

  ptr_x = malloc(20*8)
  call free(ptr_x)
end subroutine test_ok ! { dg-bogus "leak" }

subroutine test_double_free
  real*8 x(*)
  pointer(ptr_x,x)

  ptr_x = malloc(20*8)
  call free(ptr_x)
  call free(ptr_x) ! { dg-warning "double-'free'" }
end subroutine test_double_free ! { dg-bogus "leak" }
