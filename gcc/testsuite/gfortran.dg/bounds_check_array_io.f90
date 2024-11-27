! { dg-do run }
! { dg-additional-options "-fcheck=bounds -fdump-tree-original" }
!
! PR fortran/117791 - crash with bounds check writing array section
! Contributed by Andreas van Hameren (hameren at ifj dot edu dot pl)

program testprogram
  implicit none
  integer, parameter :: array(4,2)=reshape ([11,12,13,14 ,15,16,17,18], [4,2])
  integer            :: i(3) = [45,51,0]

  write(*,*) 'line 1:',array(:,          sort_2(i(1:2)) )
  write(*,*) 'line 2:',array(:,      3 - sort_2(i(1:2)) )
  write(*,*) 'line 3:',array(:, int (3 - sort_2(i(1:2))))

contains

  function sort_2(i) result(rslt)
    integer,intent(in) :: i(2)
    integer            :: rslt
    if (i(1) <= i(2)) then
       rslt = 1
    else
       rslt = 2
    endif
  end function

end program 

! { dg-final { scan-tree-dump-times "sort_2" 5 "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_transfer_array_write" "original" } }
