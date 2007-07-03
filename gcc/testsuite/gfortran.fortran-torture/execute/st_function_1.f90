! Check that character valued statement functions honour length parameters
program st_function_1
  character(8) :: foo
  character(15) :: bar
  character(6) :: p
  character (7) :: s
  foo(p) = p // "World"
  bar(p) = p // "World"
  
  ! Expression longer than function, actual arg shorter than dummy.
  call check (foo("Hello"), "Hello Wo") ! { dg-warning "Character length of actual argument shorter" }

  ! Expression shorter than function, actual arg longer than dummy.
  ! Result shorter than type
  s = "Hello"
  call check (bar(s), "Hello World    ")
contains
subroutine check(a, b)
  character (len=*) :: a, b

  if ((a .ne. b) .or. (len(a) .ne. len(b))) call abort ()
end subroutine
end program
