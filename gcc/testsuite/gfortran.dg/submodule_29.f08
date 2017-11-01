! { dg-do run }
!
! Test the fix for PR80554 in which it was not recognised that the symbol 'i'
! is host associated in the submodule 's' so that the new declaration in the
! submodule was rejected.
!
! Contributed by Tamas Bela Feher  <tamas.bela.feher@ipp.mpg.de>
!
module M
  implicit none
  integer :: i = 0
  character (100) :: buffer
  interface
    module subroutine write_i()
    end subroutine
  end interface
  interface
    module subroutine write_i_2()
    end subroutine
  end interface
contains
  subroutine foo
    integer :: i
  end
end module

submodule (M) S
    integer :: i = 137
  contains
    module subroutine write_i()
       write (buffer,*) i
    end subroutine
end submodule

submodule (M:S) S2
    integer :: i = 1037
  contains
    module subroutine write_i_2()
       write (buffer,*) i
    end subroutine
end submodule

program test_submod_variable
  use M
  implicit none
  integer :: j
  i = 42
  call write_i
  read (buffer, *) j
  if (i .ne. 42) call abort
  if (j .ne. 137) call abort
  call write_i_2
  read (buffer, *) j
  if (i .ne. 42) call abort
  if (j .ne. 1037) call abort
end program
