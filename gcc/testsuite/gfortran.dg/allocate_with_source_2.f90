! { dg-do run }
! PR 45170
! A variation of a theme for deferred type parameters.  The
! substring reference in the source= portion of the allocate
! was not probably resolved.  Testcase is a modified version
! of a program due to Hans-Werner Boschmann <boschmann at tp1
! dot physik dot uni-siegen dot de>
!
program helloworld
  character(:),allocatable::string
  real::rnd
  call hello(5, string)
  if (string /= 'hello' .or. len(string) /= 5) call abort
contains
  subroutine hello (n,string)
    character(:),allocatable,intent(out)::string
    integer,intent(in)::n
    character(20)::helloworld="hello world"
   allocate(string, source=helloworld(:n))
  end subroutine hello
end program helloworld
