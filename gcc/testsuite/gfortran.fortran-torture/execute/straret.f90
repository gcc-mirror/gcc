! Test assumed length character functions.

character*(*) function f()
  f = "Hello"
end function

character*6 function g()
  g = "World"
end function

program straret
  character*6 f, g
  character*12 v
  

  v = f() //  g()
  if (v .ne. "Hello World ") call abort ()
end program
