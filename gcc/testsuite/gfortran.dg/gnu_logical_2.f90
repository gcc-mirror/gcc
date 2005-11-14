! Testcases for the AND, OR and XOR functions (GNU intrinsics).
! { dg-do compile }
  integer i
  logical l
  real r
  complex c

  print *, and(i,i)
  print *, and(l,l)
  print *, and(i,r) ! { dg-error "must be INTEGER or LOGICAL" }
  print *, and(c,l) ! { dg-error "must be INTEGER or LOGICAL" }
  print *, and(i,l) ! { dg-error "must have the same type" }
  print *, and(l,i) ! { dg-error "must have the same type" }

  print *, or(i,i)
  print *, or(l,l)
  print *, or(i,r) ! { dg-error "must be INTEGER or LOGICAL" }
  print *, or(c,l) ! { dg-error "must be INTEGER or LOGICAL" }
  print *, or(i,l) ! { dg-error "must have the same type" }
  print *, or(l,i) ! { dg-error "must have the same type" }

  print *, xor(i,i)
  print *, xor(l,l)
  print *, xor(i,r) ! { dg-error "must be INTEGER or LOGICAL" }
  print *, xor(c,l) ! { dg-error "must be INTEGER or LOGICAL" }
  print *, xor(i,l) ! { dg-error "must have the same type" }
  print *, xor(l,i) ! { dg-error "must have the same type" }

  end
