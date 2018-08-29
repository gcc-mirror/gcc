! { dg-do run }
!
! Contributed by by Richard Maine
! http://coding.derkeiler.com/Archive/Fortran/comp.lang.fortran/2006-10/msg00104.html
!
module poly_list 

  !--  Polymorphic lists using type extension. 

  implicit none 

  type, public :: node_type 
    private 
    class(node_type), pointer :: next => null() 
  end type node_type 

  type, public :: list_type 
    private 
    class(node_type), pointer :: head => null(), tail => null() 
  end type list_type 

contains 

  subroutine append_node (list, new_node) 

    !-- Append a node to a list. 
    !-- Caller is responsible for allocating the node. 

    !---------- interface. 

    type(list_type), intent(inout) :: list 
    class(node_type), target :: new_node 

    !---------- executable code. 

    if (.not.associated(list%head)) list%head => new_node 
    if (associated(list%tail)) list%tail%next => new_node 
    list%tail => new_node 
    return 
  end subroutine append_node 

  function first_node (list) 

    !-- Get the first node of a list. 

    !---------- interface. 

    type(list_type), intent(in) :: list 
    class(node_type), pointer :: first_node 

    !---------- executable code. 

    first_node => list%head 
    return 
  end function first_node 

  function next_node (node) 

    !-- Step to the next node of a list. 

    !---------- interface. 

    class(node_type), target :: node 
    class(node_type), pointer :: next_node 

    !---------- executable code. 

    next_node => node%next 
    return 
  end function next_node 

  subroutine destroy_list (list) 

    !-- Delete (and deallocate) all the nodes of a list. 

    !---------- interface. 
    type(list_type), intent(inout) :: list 

    !---------- local. 
    class(node_type), pointer :: node, next 

    !---------- executable code. 

    node => list%head 
    do while (associated(node)) 
      next => node%next 
      deallocate(node) 
      node => next 
    end do 
    nullify(list%head, list%tail) 
    return 
  end subroutine destroy_list 

end module poly_list 

program main 

  use poly_list 

  implicit none 
  integer :: cnt

  type, extends(node_type) :: real_node_type 
    real :: x 
  end type real_node_type 

  type, extends(node_type) :: integer_node_type 
    integer :: i 
  end type integer_node_type 

  type, extends(node_type) :: character_node_type 
    character(1) :: c 
  end type character_node_type 

  type(list_type) :: list 
  class(node_type), pointer :: node 
  type(integer_node_type), pointer :: integer_node 
  type(real_node_type), pointer :: real_node 
  type(character_node_type), pointer :: character_node 

  !---------- executable code. 

  !----- Build the list. 

  allocate(real_node) 
  real_node%x = 1.23 
  call append_node(list, real_node) 

  allocate(integer_node) 
  integer_node%i = 42 
  call append_node(list, integer_node) 

  allocate(node) 
  call append_node(list, node) 

  allocate(character_node) 
  character_node%c = "z" 
  call append_node(list, character_node) 

  allocate(real_node) 
  real_node%x = 4.56 
  call append_node(list, real_node) 

  !----- Retrieve from it. 

  node => first_node(list) 

  cnt = 0
  do while (associated(node)) 
    cnt = cnt + 1
    select type (node) 
      type is (real_node_type) 
        write (*,*) node%x
        if (.not.(     (cnt == 1 .and. node%x == 1.23)   &
                  .or. (cnt == 5 .and. node%x == 4.56))) then
          STOP 1
        end if
      type is (integer_node_type) 
        write (*,*) node%i
        if (cnt /= 2 .or. node%i /= 42) STOP 2
      type is (node_type) 
        write (*,*) "Node with no data."
        if (cnt /= 3) STOP 3
      class default 
        Write (*,*) "Some other node type."
        if (cnt /= 4) STOP 4
    end select 

    node => next_node(node) 
  end do 
  if (cnt /= 5) STOP 5
  call destroy_list(list) 
  stop 
end program main 
