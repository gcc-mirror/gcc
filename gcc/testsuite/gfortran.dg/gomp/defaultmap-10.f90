subroutine f
  implicit none
  type t
    integer :: i
  end type t
  integer, target :: scalar
  integer, target :: array(5)
  integer, pointer :: ptr1, ptr2(:)
  integer, allocatable :: alloc1, alloc2(:)
  type(t) :: agg1, agg2(2)

  scalar = 1
  array = [1,2,3,4,5]
  ptr1 => scalar
  ptr2 => array
  alloc1 = 5
  alloc2 = [1,2]
  agg1%i = 1
  agg2(:)%i = [1,2]

  !$omp target defaultmap(firstprivate ) defaultmap(firstprivate : aggregate)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP with unspecified category" }
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  !$omp target defaultmap(firstprivate : all ) defaultmap(alloc : pointer)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category ALL" }
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  !$omp target defaultmap(firstprivate : aggregate)  defaultmap(firstprivate )  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category AGGREGATE" }
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  !$omp target defaultmap(alloc : pointer) defaultmap(firstprivate : all )   ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category POINTER" }
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  !$omp target defaultmap(firstprivate :all ) defaultmap(firstprivate : all)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category ALL" }
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  !$omp target defaultmap(firstprivate ) defaultmap(firstprivate)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP with unspecified category" }
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  !$omp target defaultmap(firstprivate ) defaultmap(firstprivate : all)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP with unspecified category" }
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block

  !$omp target defaultmap(firstprivate : all) defaultmap(firstprivate)  ! { dg-error "DEFAULTMAP at .1. but prior DEFAULTMAP for category ALL" }
  block
   scalar = 1;
   array(1) = 2;
   if (associated(ptr1)) &
     agg1%i = 3;
   if (associated(ptr2)) &
     agg2(1)%i = 3;
   if (allocated(alloc1)) &
     alloc2(1) = 0
  end block
end
