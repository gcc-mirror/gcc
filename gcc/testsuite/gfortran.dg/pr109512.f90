! { dg-do compile }
subroutine foo(bar)
  implicit none (external)
  call bar(1)  ! { dg-error "requires an EXTERNAL attribute" }
end subroutine foo

function bah(bar)
  implicit none (external)
  foo = bar(1)        ! { dg-error "requires an EXTERNAL attribute" }
end function bah
