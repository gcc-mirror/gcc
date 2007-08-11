! { dg-do run }
!
program test
  character(2_8) :: c(2)
  logical :: l(2)

  c = "aa"
  l = c .eq. "aa"
  if (any (.not. l)) call abort

  call foo ([c(1)])
  l = c .eq. "aa"
  if (any (.not. l)) call abort

contains

  subroutine foo (c)
    character(2) :: c(1)
  end subroutine foo

end
