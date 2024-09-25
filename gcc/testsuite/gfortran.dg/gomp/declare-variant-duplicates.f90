! Check that errors are detected if duplicates appear in name-list
! properties.

integer function f1 (x)
  integer, intent(in) :: x
  f1 = x + 1
end function
integer function f2 (x)
  integer, intent(in) :: x
  f2 = x + 2
end function
integer function f3 (x)
  integer, intent(in) :: x
  f3 = x + 3
end function
integer function f4 (x)
  integer, intent(in) :: x
  f4 = x + 4
end function

integer function f (x)
  integer, intent(in) :: x

  !$omp declare variant (f1) match (device={kind(cpu,gpu,"cpu")})  ! { dg-error "trait-property .cpu. specified more than once" }
  !$omp declare variant (f2) match (device={isa(sse4,"avx",avx)})  ! { dg-error "trait-property .avx. specified more than once" }
  !$omp declare variant (f3) match (device={arch(x86_64,i386,aarch64,"i386")})  ! { dg-error "trait-property .i386. specified more than once" }
  !$omp declare variant (f4) match (implementation={vendor(llvm,gnu,"arm",gnu)})  ! { dg-error "trait-property .gnu. specified more than once" }

  f = x
end function  
