! { dg-do compile }
! { dg-options "-w -fdump-tree-original" }
!
! Contributed by Sebastian Bardeau  <bardeau@iram.fr>
!
module mymod
  type mysubtype
    integer(kind=4), allocatable :: a(:)
  end type mysubtype
  type :: mytype
    integer :: i
    type(mysubtype) :: sub
  contains
    final :: mytype_final
  end type mytype
contains
  subroutine mysubtype_final(sub)
    type(mysubtype), intent(inout) :: sub
    print *,'MYSUBTYPE>FINAL'
    if (allocated(sub%a)) deallocate(sub%a)
  end subroutine mysubtype_final
  subroutine mytype_final(typ)
    type(mytype), intent(inout) :: typ
    print *,"MYTYPE>FINAL"
    call mysubtype_final(typ%sub)
  end subroutine mytype_final
end module mymod
!
program myprog
  use mymod
  type(mytype), pointer :: c
  print *,"Before allocation"
  allocate(c)
  print *,"After allocation"
end program myprog
! Final subroutines were called with std=gnu and -w = > 14 "_final"s.
! { dg-final { scan-tree-dump-times "_final" 12 "original" } }
