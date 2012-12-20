! { dg-do compile }
!
! PR fortran/54818
!
! Contributed by  Scott Pakin
!
      subroutine broken ( name1, name2, bmix )

      implicit none

      integer, parameter :: i_knd  = kind( 1 )
      integer, parameter :: r_knd  = selected_real_kind( 13 )

      character(len=8) :: dum
      character(len=8) :: blk
      real(r_knd), dimension(*) :: bmix, name1, name2
      integer(i_knd) :: j, idx1, n, i
      integer(i_knd), external :: nafix

      write (*, 99002) name1(j),
     &     ( adjustl(
     &     transfer(name2(nafix(bmix(idx1+i),1)),dum)//blk
     &     //blk), bmix(idx1+i+1), i = 1, n, 2 )

99002 format (' *', 10x, a8, 8x, 3(a24,1pe12.5,',',6x))

      end subroutine broken
