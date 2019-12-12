! { dg-do compile }
! { dg-additional-options "-pedantic" }
! PR fortran/89981 - this used to give a wrong warning (error with
! -pedantic)
program main
  call bar(i)
  call baz(i) ! { dg-error "Type mismatch in argument" }
end program main
subroutine foo(r)
  entry bar(i)
  entry baz(r)
end subroutine foo
