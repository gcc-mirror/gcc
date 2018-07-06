! { dg-do compile }
elemental function f() result(s) ! { dg-error "shall not have an ALLOCATABLE or POINTER" }
  allocatable s
  allocate(s)
  s = 3.5
end function

elemental function g() result(s) ! { dg-error "shall not have an ALLOCATABLE or POINTER" }
  pointer s
  allocate(s)
  s = 3.5
end function
