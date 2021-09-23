pure elemental subroutine foo()
!$acc routine vector  ! { dg-error "ROUTINE with GANG, WORKER, or VECTOR clause is not permitted in PURE procedure" }
end

pure elemental subroutine foo_nh()
!$acc routine nohost vector  ! { dg-error "ROUTINE with GANG, WORKER, or VECTOR clause is not permitted in PURE procedure" }
end

elemental subroutine foo2()
!$acc routine (myfoo2) gang  ! { dg-error "Invalid NAME 'myfoo2' in" }
end

elemental subroutine foo2a()
!$acc routine gang  ! { dg-error "ROUTINE with GANG, WORKER, or VECTOR clause is not permitted in PURE procedure" }
end

elemental subroutine foo2a_nh()
!$acc routine nohost gang  ! { dg-error "ROUTINE with GANG, WORKER, or VECTOR clause is not permitted in PURE procedure" }
end

pure subroutine foo3()
!$acc routine vector ! { dg-error "ROUTINE with GANG, WORKER, or VECTOR clause is not permitted in PURE procedure" }
end

pure subroutine foo3_nh()
!$acc routine nohost vector ! { dg-error "ROUTINE with GANG, WORKER, or VECTOR clause is not permitted in PURE procedure" }
end

elemental impure subroutine foo4()
!$acc routine vector ! OK: impure
end

elemental impure subroutine foo4_nh()
!$acc routine nohost vector ! OK: impure
end

pure subroutine foo5()
!$acc routine seq ! OK: seq
end

pure subroutine foo5_nh()
!$acc routine nohost seq ! OK: seq
end

pure subroutine foo6()
!$acc routine ! OK (implied 'seq')
end

pure subroutine foo6_nh()
!$acc routine nohost ! OK (implied 'seq')
end
