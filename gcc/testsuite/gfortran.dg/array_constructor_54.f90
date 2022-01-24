! { dg-do compile }
! { dg-options "-fdump-tree-original -Warray-temporaries" }
! { dg-final { scan-tree-dump-not "stride" "original" } }
! Verify that no temporary array is generated for a constant array constructor
! See e.g. PR fortran/102717, PR fortran/102787

program p
  integer, parameter :: a(*)   = [1,2,3,4]
  integer, parameter :: b(2,3) = reshape([1,2,3,4,5,6], shape (b))
  print *, [a]
  print *, [a( : ) ]
  print *, [a( ::1)]
  print *, [a( ::2)]
  print *, [a(1:2:1)]
  print *, [a(4:1:-2)]
  print *, [a([3,2])]
  print *, [a,1]
  print *, [1,a]
  print *, [a,a]
  print *, [b(:,3:1:-2)]
  print *, [1,b(1,[2,1,3])]
  print *, [a,b]
end
