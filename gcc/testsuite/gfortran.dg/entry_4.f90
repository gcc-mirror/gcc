! { dg-do compile { target i?86-*-* x86_64-*-* } }
function f1 () result (r)      ! { dg-error "cannot be a POINTER" }
integer, pointer :: r
real e1
allocate (r)
r = 6
return
entry e1 ()
e1 = 12
entry e1a ()
e1a = 13
end function
function f2 ()
integer, dimension (2, 7, 6) :: e2   ! { dg-error "cannot be an array" }
f2 = 6
return
entry e2 ()
e2 (:, :, :) = 2
end function
integer(kind=8) function f3 ()      ! { dg-error "cannot be of type" }
complex(kind=8) e3              ! { dg-error "cannot be of type" }
f3 = 1
return
entry e3 ()
e3 = 2
entry e3a ()
e3a = 3
end function
