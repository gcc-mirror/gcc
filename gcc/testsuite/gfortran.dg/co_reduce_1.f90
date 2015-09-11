! { dg-do compile }
! { dg-additional-options "-fdump-tree-original -fcoarray=lib" }
!
! Check that we don't take twice the address of procedure simple_reduction
! in the generated code.
!
! Contributed by Alessandro Fanfarillo <fanfarillo.gcc@gmail.com>

program simple_reduce
  implicit none

  integer :: me

  me = this_image()

  sync all

  call co_reduce(me,simple_reduction)

  write(*,*) this_image(),me

contains
  
  pure function simple_reduction(a,b)
    integer,intent(in) :: a,b
    integer :: simple_reduction

    simple_reduction = a * b
  end function simple_reduction

end program simple_reduce

! { dg-final { scan-tree-dump "_gfortran_caf_co_reduce \\(&desc\\.\\d+,\\s*simple_reduction," "original" } }
