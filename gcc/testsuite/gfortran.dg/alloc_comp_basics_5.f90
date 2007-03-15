! { dg-do run }
! This checks the correct functioning of derived types with the SAVE
! attribute and allocatable components - PR31163
!
! Contributed by Salvatore Filippone  <salvatore.filippone@uniroma2.it>
!
Module bar_mod

  type foo_type
     integer, allocatable :: mv(:)
  end type foo_type


contains


  subroutine bar_foo_ab(info)

    integer, intent(out)               :: info
    Type(foo_type), save :: f_a
    
    if (allocated(f_a%mv)) then 
      info = size(f_a%mv)
    else
      allocate(f_a%mv(10),stat=info)
      if (info /= 0) then 
        info = -1 
      endif
    end if
  end subroutine bar_foo_ab


end module bar_mod

program tsave
  use bar_mod

  integer :: info
  
  call bar_foo_ab(info) 
  if (info .ne. 0) call abort ()
  call bar_foo_ab(info) 
  if (info .ne. 10) call abort ()
  
end program tsave

! { dg-final { cleanup-modules "bar_mod" } }
