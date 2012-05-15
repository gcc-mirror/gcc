! { dg-do compile }
! I couldn't reproduce the failure with a compiler built from the
! 2004-09-26 sources
      module specfiles
      contains
      subroutine split(instring,outstrings,lenout,n,i)
      integer(kind=4),intent(in) :: lenout,n
      character(len=*),intent(in) :: instring
      character(len=lenout),dimension(n),intent(out) :: outstrings
      integer(kind=4) :: i,j,k
      j=1; k=1
       outstrings(j)(k:k)=instring(i:i)
      return
      end subroutine split
      end module specfiles
