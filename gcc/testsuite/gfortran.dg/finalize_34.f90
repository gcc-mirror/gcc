! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! PR 87352 - this used to cause an excessive number of deallocations.
module testmodule
  implicit none
  public

  type :: evtlist_type
     real,  allocatable, dimension(:) :: p1
     real,  allocatable, dimension(:) :: p2
     real,  allocatable, dimension(:) :: p3
     real,  allocatable, dimension(:) :: p4
  end type evtlist_type

  type :: evtlistlist_type
     type(evtlist_type)  :: evtlist(1:1)
  end type evtlistlist_type

end module testmodule 

program main
  use testmodule
  type(evtlist_type), dimension(10) :: a
end program main
! { dg-final  { scan-tree-dump-times "__builtin_free" 8 "original" } }
