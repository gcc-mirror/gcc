! { dg-do run }
! This verifies that the compiler will correctly accpet the name="", write out
! an empty string for the binding label to the module file, and then read it
! back in.  Also, during gfc_verify_binding_labels, the name="" will prevent
! any verification (since there is no label to verify).
module one
contains
  subroutine foo() bind(c)
  end subroutine foo
end module one

module two
contains
  ! This procedure is only used accessed in C
  ! as procedural pointer
  subroutine foo() bind(c, name="")
  end subroutine foo
end module two

use one, only: foo_one => foo
use two, only: foo_two => foo
end

! { dg-final { cleanup-modules "one two" } }
