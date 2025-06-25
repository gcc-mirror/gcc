!{ dg-do run }

! Contributed by Christopher Albert  <albert@tugraz.at>

program grow_type_array
    type :: container
        integer, allocatable :: arr(:)
    end type container
    
    type(container), allocatable :: list(:)

    list = [list, new_elem(5)]

    deallocate(list)

contains

    type(container) function new_elem(s) result(out)
        integer :: s
        allocate(out%arr(s))
    end function new_elem
      
end program grow_type_array
