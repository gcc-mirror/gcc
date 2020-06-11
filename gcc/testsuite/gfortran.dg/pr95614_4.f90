! { dg-do compile }

function f()
  f = 1.0
end function

program pr95614
  common /c1/ f
end program
