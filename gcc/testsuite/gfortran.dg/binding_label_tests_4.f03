! { dg-do compile }
module A
  use, intrinsic :: iso_c_binding
contains
  subroutine pA() bind(c, name='printf') ! { dg-error "Procedure 'pb' with binding label 'printf' at .1. uses the same global identifier as entity at .2." }
    print *, 'hello from pA'
  end subroutine pA
end module A

module B
  use, intrinsic :: iso_c_binding

contains
  subroutine pB() bind(c, name='printf') ! { dg-error "Procedure 'pb' with binding label 'printf' at .1. uses the same global identifier as entity at .2." }
    print *, 'hello from pB'
  end subroutine pB
end module B

module C
use A
use B ! { dg-error "Cannot open module file" }
end module C
! { dg-prune-output "compilation terminated" }
