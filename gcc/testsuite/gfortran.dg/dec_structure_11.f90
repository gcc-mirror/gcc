! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! Tests for what CAN'T be done with dot ('.') as a member accessor.
!

structure /s1/
  integer eq
end structure

record /s1/ r
integer i, j, k

j = i.j          ! { dg-error "nonderived-type variable" }
j = r .eq. i     ! { dg-error "Operands of comparison" }
j = r.i          ! { dg-error "is not a member of" }
j = r.           ! { dg-error "Expected structure component or operator name" }
j = .i           ! { dg-error "Invalid character in name" }

end
