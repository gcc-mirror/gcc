! Check that errors are detected if other trait-properties are given with
! kind(any).

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
integer function f5 (x)
  integer, intent(in) :: x
  f4 = x + 5
end function
integer function f6 (x)
  integer, intent(in) :: x
  f4 = x + 6
end function

integer function f (x)
  integer, intent(in) :: x

  !$omp declare variant (f1) match (device={kind(any,gpu)})  ! { dg-error "no other trait-property may be specified" }
  !$omp declare variant (f2) match (device={kind(cpu,"any")})  ! { dg-error "no other trait-property may be specified" }
  !$omp declare variant (f3) match (device={kind("any"),arch(x86_64)})  ! { dg-error "no other trait-property may be specified" }
  !$omp declare variant (f4) match (device={arch(x86_64),kind(any)})  ! { dg-error "no other trait-property may be specified" }
  !$omp declare variant (f5) match (device={kind(any,"any")})  ! { dg-error "trait-property .any. specified more than once" }
  !$omp declare variant (f6) match (device={kind("any",any)})  ! { dg-error "trait-property .any. specified more than once" }

  f = x
end function  
