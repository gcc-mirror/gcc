! { dg-do compile }
implicit none
integer ::x,z
x = 42
print '(*(z16:" "))', loc(x)
!$omp target map(x, z)
block
  integer :: y
  x = 123
  y = 99
  !$omp target device(ancestor:1) map(always,tofrom:x) map(y) ! { dg-error "'ancestor' device modifier not preceded by 'requires' directive with 'reverse_offload' clause" }
    print '(*(z16:" "))', loc(x), loc(y)
    print * ,x, y
    x = -x
    y = -y
  !$omp end target ! { dg-error "Unexpected ..OMP END TARGET statement" }
  z = y
end block
    print * ,x !, z
end

