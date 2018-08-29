! { dg-do compile }
!
! Tests the fix for PR70864 in which compiler generated temporaries received
! the attributes of a dummy argument. This is the original testcase.
! The simplified version by Gerhard Steinmetz is gratefully acknowledged.
!
! Contributed by Weiqun Zhang  <weiqun.zhang@gmail.com>
!
module boxarray_module
  implicit none
  type :: BoxArray
     integer     :: i = 0
   contains
     procedure ::                  boxarray_assign
     generic   :: assignment(=) => boxarray_assign
  end type BoxArray
contains
  subroutine boxarray_assign (dst, src)
    class(BoxArray), intent(inout) :: dst
    type (BoxArray), intent(in   ) :: src
    dst%i =src%i
  end subroutine boxarray_assign
end module boxarray_module

module multifab_module
  use boxarray_module
  implicit none
  type, public   :: MultiFab
     type(BoxArray) :: ba
  end type MultiFab
contains
  subroutine multifab_swap(mf1, mf2)
    type(MultiFab), intent(inout) :: mf1, mf2
    type(MultiFab) :: tmp
    tmp = mf1
    mf1 = mf2 ! Generated an ICE in trans-decl.c.
    mf2 = tmp
  end subroutine multifab_swap
end module multifab_module
