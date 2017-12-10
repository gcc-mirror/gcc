! { dg-do compile }
! PR53478

module test_bug ! { dg-error "Procedure 'test' with binding label 'Test_Bug' at .1. uses the same global identifier as entity at .2." }

use, intrinsic :: ISO_C_BINDING

contains

  subroutine test() bind (C, name = "Test_Bug") ! { dg-error "Procedure 'test' with binding label 'Test_Bug' at .1. uses the same global identifier as entity at .2." }
  end subroutine

end module
