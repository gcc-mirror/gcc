! { dg-do compile }
!

function f()
  intrinsic :: sin
  procedure(sin), pointer :: f ! { dg-error "Procedure pointer 'f'" }
  f => sin
end function f
