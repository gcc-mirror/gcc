! { dg-do compile }
! I couldn't reproduce the failure with a compiler built from the
! 2004-09-26 sources
      module specfiles
      contains
      subroutine split(i,o,lenout,n)
      integer(kind=4),intent(in) :: lenout,n
      character(len=*),intent(in) :: i
      character(len=lenout),dimension(n),intent(out) :: o
      integer(kind=4) :: j,k,l
      j=1; k=1
       outstrings(j)(k:k)=instring(i:i)
      return
      end subroutine split
      end module specfiles
