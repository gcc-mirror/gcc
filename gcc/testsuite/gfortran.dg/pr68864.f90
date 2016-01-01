! { dg-do compile }
!
! Contributed by Hossein Talebi  <talebi.hossein@gmail.com>
!
!
Module part_base2_class

    implicit none

    type :: ty_moc1
        integer l
    end type ty_moc1
    integer,parameter ::  MAX_NUM_ELEMENT_TYPE=32

    type :: ty_element_index2

        class(ty_moc1),allocatable :: element
        class(ty_moc1),allocatable :: element_th(:)

    endtype ty_element_index2

    type :: ty_part_base2
        type(ty_element_index2)::element_index(MAX_NUM_ELEMENT_TYPE)
    end type ty_part_base2

    class(ty_part_base2),allocatable ::  part_tmp_obj

End Module part_base2_class

    use part_base2_class
    allocate (part_tmp_obj)
    allocate (part_tmp_obj%element_index(1)%element, source = ty_moc1(1))
    allocate (part_tmp_obj%element_index(1)%element_th(1), source = ty_moc1(99))
    allocate (part_tmp_obj%element_index(32)%element_th(1), source = ty_moc1(999))

    do i = 1, MAX_NUM_ELEMENT_TYPE
      if (allocated (part_tmp_obj%element_index(i)%element_th)) then
        print *, i, part_tmp_obj%element_index(i)%element_th(1)%l
      end if
    end do
    deallocate (part_tmp_obj)

end
