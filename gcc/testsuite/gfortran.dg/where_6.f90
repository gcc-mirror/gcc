! { dg-do compile }
! { dg-options "-Wcharacter-truncation" }
subroutine where_ice (i,j)
     
  implicit none

  character(8)  :: y(10,10,2)

  integer       :: i
  integer       :: j

  character(12) :: txt(5)
  if (.true.) where (txt(1:3) /= ''   )  y(1:3,i,j) = txt(1:3) ! { dg-warning "CHARACTER expression will be truncated" }

end subroutine where_ice
