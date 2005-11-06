! { dg-do compile }
! { dg-options "-std=f95" }
! Tests fix for PR17737 - already initialized variable cannot appear
! in data statement
      integer :: i, j = 1
      data i/0/
      data i/0/   ! { dg-error "Extension: re-initialization" }
      data j/2/   ! { dg-error "Extension: re-initialization" }
      end

